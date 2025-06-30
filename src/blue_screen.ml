open Core

(* You need to change the implementation of this function so that it
   replaces the "blue" pixels of the foreground image with pixels from
   the corresponding position in the background image instead of
   just ignoring the background image and returning the foreground image.
*)

let is_sufficiently_blue pixel =
  Pixel.blue pixel > Pixel.red pixel + Pixel.green pixel
;;

let transform ~foreground ~background =
  Image.mapi foreground ~f:(fun ~x ~y foreground_pixel ->
    match is_sufficiently_blue foreground_pixel with
    | true -> Image.get background ~x ~y
    | false -> foreground_pixel)
;;

let%expect_test "transform blue screen" = 
  let foreground =
    Image.load_ppm ~filename:"../images/oz_bluescreen.ppm"
  in
  let background =
    Image.load_ppm ~filename:"../images/meadow.ppm"
  in
  let output_image = transform ~foreground ~background in
  let expected_output = Image.load_ppm ~filename:"../images/reference-oz_bluescreen_vfx.ppm" in
  print_s [%message "transformed image height" ~_:(Image.height output_image : int)];
  [%expect {|("transformed image height" 440)|}];
  print_s [%message "transformed image width" ~_:(Image.width output_image : int)];
  [%expect {|("transformed image width" 880)|}];
  let pixels_diff = Image.foldi output_image ~init:0 ~f:(fun ~x ~y pixels_diff pixel ->
    let expected_pixel = Image.get expected_output ~x ~y in
    match Pixel.equal expected_pixel pixel with
    | true -> pixels_diff
    | false -> pixels_diff + 1) in
  print_s [%message "# of pixels different" ~_:(pixels_diff: int)];
  [%expect {| ("# of pixels different" 0) |}];
  ;;

let command =
  Command.basic
    ~summary:
      "Replace the 'blue' pixels of an image with those from another image"
    [%map_open.Command
      let foreground_file =
        flag
          "foreground"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the foreground PPM image file"
      and background_file =
        flag
          "background"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the background PPM image file"
      in
      fun () ->
        let foreground = Image.load_ppm ~filename:foreground_file in
        let background = Image.load_ppm ~filename:background_file in
        let image' = transform ~foreground ~background in
        Image.save_ppm
          image'
          ~filename:
            (String.chop_suffix_exn foreground_file ~suffix:".ppm"
             ^ "_vfx.ppm")]
;;
