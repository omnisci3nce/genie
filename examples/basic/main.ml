[@@@warning "-26-27-32-33-34-37-69"]

open Genie.Redux
open Genie.Components
open Genie.Redux.Styles
open Raylib.Color
open CCHashtbl

module ChatApp = struct
  type message = { author : string; contents : string }

  type chat_model = {
    selected_channel : int option;
    channels : string list;
    messages : (string, message list) Hashtbl.t;
  }

  let build theme model =
    let module Theme = (val theme : Design.Theme) in
    let channel_list =
      col
        (List.mapi
           (fun index ch_name ->
             list_item
               ~styles:
                 (default_style
                 |> padding (Spacing.merge (Theme.hori_space S_Sm) (Theme.vert_space S_Xs))
                 |> bg_color (Theme.neutral C_5)
                 |> hover_color (Theme.neutral C_3))
               (!model.selected_channel = Some index)
               (fun () ->
                 Printf.printf "Selected channel %s\n" ch_name;
                 model := { !model with selected_channel = Some index })
               ch_name)
           !model.channels)
    in
    let sidebar =
      col
        ~size:{ x_axis = Fixed 150; y_axis = Fixed 500 }
        ~styles:
          (default_style
          |> bg_color (Theme.neutral C_4)
          |> padding (Spacing.merge (Theme.hori_space S_Sm) (Theme.vert_space S_Md)))
        [ text_builder ~weight:Bold ~color:(Theme.neutral C_12) "Channels"; channel_list ]
    in
    let ch_name = Option.get !model.selected_channel |> List.nth !model.channels in
    let messages = Hashtbl.find_opt !model.messages ch_name |> Option.value ~default:[] in
    let message_list =
      col
        ~styles:(default_style |> bg_color blue)
        (List.map (fun msg -> text_builder msg.contents) messages)
    in
    let new_message_form =
      Textbox.make ~stable_key:(StableId.Int 666) ~placeholder:" IHIHI Placeholder   " ()
    in
    let header =
      !model.selected_channel
      |> Option.map (fun idx -> List.nth !model.channels idx)
      |> Option.value ~default:"" |> text_builder ~weight:Bold
    in
    let main_area =
      col
        ~styles:
          (default_style |> padding @@ Spacing.merge (Theme.hori_space S_Lg) (Theme.vert_space S_Md))
        [ header; message_list; new_message_form ]
    in
    row ~styles:(default_style |> bg_color orange) [ sidebar; main_area ]
end

let () =
  let open ChatApp in
  let msgs = Hashtbl.create 100 in
  Hashtbl.add msgs "General"
    [
      { author = "Joshua"; contents = "Good morning!" };
      { author = "Joshua"; contents = "Coffee time. lets goooo" };
    ];
  let initial_model =
    ref
      {
        selected_channel = Some 0;
        channels = [ "General"; "Programming"; "Interesting-Finds" ];
        messages = msgs;
      }
  in
  let app = App.setup (600, 500) in
  App.run app initial_model ChatApp.build
