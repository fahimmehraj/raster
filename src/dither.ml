open Core

type adjacent_pixel =
  { multiplier : float
  ; direction : int * int
  }

let adjacents image ~x ~y =
  [ { multiplier = 7. /. 16.; direction = x + 1, y }
  ; { multiplier = 3. /. 16.; direction = x - 1, y + 1 }
  ; { multiplier = 5. /. 16.; direction = x, y + 1 }
  ; { multiplier = 1. /. 16.; direction = x + 1, y + 1 }
  ]
  |> List.filter ~f:(fun { direction = x, y; _ } ->
    x >= 0 && x < Image.width image && y >= 0 && y < Image.height image)
;;

let transform image =
  let image = image |> Grayscale.transform in
  (* Use iter instead of map since with each function call we perform the added side effect
     of modifying adjacent cells *)
  Image.iteri image ~f:(fun ~x ~y pixel ->
    let new_pixel =
      match Pixel.red pixel > Image.max_val image / 2 with
      | false -> 0, 0, 0
      | true -> Image.max_val image, Image.max_val image, Image.max_val image
    in
    let error = Float.of_int (Pixel.red pixel - Pixel.red new_pixel) in
    Image.set image ~x ~y new_pixel;
    List.iter
      (adjacents image ~x ~y)
      ~f:(fun { multiplier; direction = x, y } ->
        let adjacent_pixel = Image.get image ~x ~y in
        let pixel_to_add =
          Pixel.of_int (Int.of_float (Float.round (error *. multiplier)))
        in
        Image.set image ~x ~y Pixel.(adjacent_pixel + pixel_to_add)));
  image
;;

let%expect_test "transform dither" =
  let input_image =
    Image.load_ppm ~filename:"../images/beach_portrait.ppm"
  in
  let output_image = transform input_image in
  let expected_output =
    Image.load_ppm ~filename:"../images/reference-beach_portrait_dither.ppm"
  in
  Image.compare_images_helper output_image expected_output;
  [%expect {|"images are identical"|}]
;;

let command =
  Command.basic
    ~summary:"Dither an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      in
      fun () ->
        let image = Image.load_ppm ~filename |> transform in
        Image.save_ppm
          image
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_dither.ppm")]
;;
