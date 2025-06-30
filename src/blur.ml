open Core

(* You need to modify this function to blur the input image
   based on the provided radius instead of ignoring it. *)
let transform image ~radius =
  Image.mapi image ~f:(fun ~x ~y _ ->
    let y_start = max (y - radius) 0 in
    let y_end = min (y + radius) (Image.height image - 1) in
    let x_start = max (x - radius) 0 in
    let x_end = min (x + radius) (Image.width image - 1) in
    Image.slice image ~x_start ~x_end ~y_start ~y_end |> Image.mean_pixel)
;;

let%expect_test "transform blur" = 
  let input_image =
    Image.load_ppm ~filename:"../images/beach_portrait.ppm"
  in
  let output_image = transform input_image ~radius:3 in
  let expected_output = Image.load_ppm ~filename:"../images/reference-beach_portrait_blur.ppm" in
  Image.compare_images_helper output_image expected_output;
  [%expect {|"images are identical"|}]
  ;;

let command =
  Command.basic
    ~summary:"Blur an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      and radius =
        flag
          "radius"
          (required Command.Param.int)
          ~doc:"N the radius to use when blurring (higher = more blurred)"
      in
      fun () ->
        let image = Image.load_ppm ~filename in
        let image' = transform image ~radius in
        Image.save_ppm
          image'
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_blur.ppm")]
;;
