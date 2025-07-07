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

let clamped_color ~colors_per_channel ~max_val color =
  if colors_per_channel <= 1
  then
    raise_s
      [%message
        "cannot dither with so few colors" (colors_per_channel : int)];
  let step = max_val / (colors_per_channel - 1) in
  Int.round_nearest ~to_multiple_of:step color
;;

let dithered_pixel ~colors_per_channel ~max_val pixel =
  let clamped_color = clamped_color ~colors_per_channel ~max_val in
  ( clamped_color (Pixel.red pixel)
  , clamped_color (Pixel.green pixel)
  , clamped_color (Pixel.blue pixel) )
;;

let transform ?(colors_per_channel = 2) ?(grayscale = true) image =
  let image =
    match grayscale with true -> Grayscale.transform image | false -> image
  in
  (* Use iter instead of map since with each function call we perform the added side effect
     of modifying adjacent cells *)
  Image.iteri image ~f:(fun ~x ~y pixel ->
    let new_pixel =
      dithered_pixel ~colors_per_channel ~max_val:(Image.max_val image) pixel
    in
    let error = Pixel.(pixel - new_pixel) in
    Image.set image ~x ~y new_pixel;
    List.iter
      (adjacents image ~x ~y)
      ~f:(fun { multiplier; direction = x, y } ->
        let adjacent_pixel = Image.get image ~x ~y in
        let pixel_to_add = Pixel.scalar_product error ~scalar:multiplier in
        Image.set image ~x ~y Pixel.(adjacent_pixel + pixel_to_add)));
  image
;;

let%expect_test "grayscale transform dither" =
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

let%expect_test "color transform dither" =
  let input_image =
    Image.load_ppm ~filename:"../images/beach_portrait.ppm"
  in
  let output_image =
    transform ~colors_per_channel:2 ~grayscale:false input_image
  in
  let expected_output =
    Image.load_ppm
      ~filename:"../images/reference-beach_portrait_dither_color.ppm"
  in
  Image.compare_images_helper output_image expected_output;
  [%expect {|"images are identical"|}]
;;

let%expect_test "illegal inputs to transform" =
  Expect_test_helpers_core.require_does_raise [%here] (fun () ->
    let input_image =
      Image.load_ppm ~filename:"../images/beach_portrait.ppm"
    in
    let _output =
      transform ~colors_per_channel:1 ~grayscale:false input_image
    in
    ());
  [%expect {|("cannot dither with so few colors" (colors_per_channel 1))|}]
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
      and colors_per_channel =
        flag
          "colors-per-channel"
          (optional_with_default 2 Command.Param.int)
          ~doc:"N the number of colors to choose from each channel"
      and grayscale =
        flag
          "grayscale"
          no_arg
          ~doc:"whether or not the dithered image should be black and white"
      in
      fun () ->
        let image =
          Image.load_ppm ~filename
          |> transform ~grayscale ~colors_per_channel
        in
        Image.save_ppm
          image
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_dither.ppm")]
;;
