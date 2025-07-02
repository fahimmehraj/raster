open Core


let is_sufficiently_blue pixel =
  let blue_val = Float.of_int (Pixel.blue pixel) in
  let red_val = Float.of_int (Pixel.red pixel) in
  let green_val = Float.of_int (Pixel.green pixel) in
  Float.(blue_val >. (1.1 *. red_val)) && Float.(blue_val >. (1.1 *. green_val)) && Float.(blue_val >. 18000.)
;;

let is_region_sufficiently_blue image ~x ~y ~radius =
  let y_start = max (y - radius) 0 in
  let y_end = min (y + radius) (Image.height image - 1) in
  let x_start = max (x - radius) 0 in
  let x_end = min (x + radius) (Image.width image - 1) in
  let avg_color = Image.slice image ~x_start ~x_end ~y_start ~y_end |> Image.mean_pixel in
  is_sufficiently_blue avg_color

let transform ~foreground ~background =
  Image.mapi foreground ~f:(fun ~x ~y foreground_pixel ->
    match is_region_sufficiently_blue foreground ~x ~y ~radius:1 with
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
  Image.fuzzy_compare_images_helper ~radius:4 output_image expected_output;
  [%expect {|"images are identical"|}]
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
