open Core


(* You need to change the implementation of this function so that it does something
   to the image instead of just leaving it untouched. *)
let transform (image : Image.t) =
  Image.map image ~f:(fun pixel ->
    let average_val =
      (Pixel.red pixel + Pixel.blue pixel + Pixel.green pixel) / 3
    in
    average_val, average_val, average_val)
;;

let%expect_test "transform grayscale" =
  let input_image =
    Image.load_ppm ~filename:"../images/beach_portrait.ppm"
  in
  let output_image = transform input_image in
  let expected_image =
    Image.load_ppm ~filename:"../images/reference-beach_portrait_gray.ppm"
  in
  Image.compare_images_helper output_image expected_image;
  [%expect {|"images are identical"|}]
;;

let command =
  Command.basic
    ~summary:"Convert an image to grayscale"
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
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_gray.ppm")]
;;
