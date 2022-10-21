module Main exposing (main)

import Base64
import Browser
import Codec
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes
import List.Extra
import Material.Icons
import Material.Icons.Types
import String.Extra
import Url


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Flags =
    { locationHref : String
    }


type alias Model =
    { permanentState : PermanentState
    , modality : Modality
    }


type alias PermanentState =
    { x : String
    , y : String
    , attendees : Dict.Dict String String
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        initPermanentState : PermanentState
        initPermanentState =
            { x = "4"
            , y = "3"
            , attendees = Dict.fromList [ ( "1", "Mark" ), ( "2", "John" ) ]
            }

        permanentState : PermanentState
        permanentState =
            flags.locationHref
                |> Url.fromString
                |> Maybe.map .path
                |> Maybe.withDefault ""
                |> String.dropLeft 1
                |> Base64.decode
                |> Result.withDefault ""
                |> Codec.decodeString codecPermanentState
                |> Result.withDefault initPermanentState
    in
    ( { permanentState = permanentState
      , modality = ModalityNormal
      }
    , Cmd.none
    )


type Modality
    = ModalityNormal
    | ModalitySettings
    | ModalityEditing
    | ModalityFullscreen Int


codecPermanentState : Codec.Codec PermanentState
codecPermanentState =
    Codec.object
        (\x y attendees ->
            { x = x
            , y = y
            , attendees = attendees
            }
        )
        |> Codec.field "x" .x Codec.string
        |> Codec.field "y" .y Codec.string
        |> Codec.field "a" .attendees (Codec.dict Codec.string)
        |> Codec.buildObject


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        permanentState : PermanentState
        permanentState =
            model.permanentState
    in
    case msg of
        MsgChangeX string ->
            ( { model | permanentState = { permanentState | x = string } }, Cmd.none )

        MsgChangeY string ->
            ( { model | permanentState = { permanentState | y = string } }, Cmd.none )

        MsgChangeAttendee id name ->
            ( { model
                | permanentState =
                    { permanentState
                        | attendees = Dict.insert id name permanentState.attendees
                    }
              }
            , Cmd.none
            )

        MsgChangeModality modality ->
            ( { model | modality = modality }, Cmd.none )


type Msg
    = MsgChangeX String
    | MsgChangeY String
    | MsgChangeAttendee String String
    | MsgChangeModality Modality


primaryColor : Color
primaryColor =
    rgb 0.8 0.3 0


menuAttrs : List (Attribute msg)
menuAttrs =
    [ spacing 5
    , Font.color primaryColor
    , Background.color <| rgba 1 1 1 0.5
    , Border.rounded 10
    , padding 5
    , moveDown 10
    ]


iconMenu : Model -> Int -> Attribute Msg
iconMenu model id_ =
    let
        id : Int
        id =
            case model.modality of
                ModalityFullscreen int ->
                    int

                _ ->
                    id_
    in
    inFront <|
        row
            (menuAttrs ++ [ moveRight 10 ])
            ([ el
                [ Font.size 36
                , Font.bold
                ]
                (text <| String.fromInt id)
             ]
                ++ (case model.modality of
                        ModalityNormal ->
                            [ Input.button [] { label = el [] <| html <| Material.Icons.open_in_full 30 Material.Icons.Types.Inherit, onPress = Just <| MsgChangeModality <| ModalityFullscreen id }
                            , Input.button [] { label = el [] <| html <| Material.Icons.edit 30 Material.Icons.Types.Inherit, onPress = Just <| MsgChangeModality <| ModalityEditing }
                            , el [] <| html <| Material.Icons.open_in_new 30 Material.Icons.Types.Inherit
                            , el [] <| html <| Material.Icons.settings 30 Material.Icons.Types.Inherit
                            ]

                        ModalityFullscreen _ ->
                            []

                        _ ->
                            []
                   )
                ++ [ el [ Font.size 28, moveDown 3 ] <| text <| Maybe.withDefault "" <| Dict.get (String.fromInt id) model.permanentState.attendees
                   ]
            )


view : Model -> Html.Html Msg
view model =
    let
        permanentState : PermanentState
        permanentState =
            model.permanentState

        { x, y } =
            permanentStateToXY permanentState

        spacingSize =
            10
    in
    layout
        ([]
            ++ (case model.modality of
                    ModalityFullscreen int ->
                        [ inFront <|
                            el
                                [ width fill
                                , height fill
                                , padding 100
                                , Background.color <| rgba 0 0 0 0.4
                                ]
                            <|
                                el
                                    [ width fill
                                    , height fill
                                    , Background.color <| rgb 1 1 1
                                    , Border.width spacingSize
                                    , Border.color primaryColor
                                    , iconMenu model 0
                                    , inFront <|
                                        el (menuAttrs ++ [ alignRight, moveLeft 10 ]) <|
                                            Input.button []
                                                { label = el [] <| html <| Material.Icons.close_fullscreen 30 Material.Icons.Types.Inherit
                                                , onPress = Just <| MsgChangeModality <| ModalityNormal
                                                }
                                    ]
                                <|
                                    html <|
                                        Html.iframe
                                            [ Html.Attributes.style "border" "0"
                                            , Html.Attributes.style "height" "100%"
                                            , Html.Attributes.style "background" "#4a0"

                                            -- , Html.Attributes.src "https://07-elm-boot.lucamug.repl.co/"
                                            , Html.Attributes.src "https://example.com/"

                                            -- , Html.Attributes.src "https://repubblica.it/"
                                            ]
                                            []
                        ]

                    _ ->
                        []
               )
        )
    <|
        column
            [ width fill
            , height fill
            , Background.color primaryColor
            , spacing spacingSize
            , padding spacingSize
            ]
            (List.indexedMap
                (\indexX _ ->
                    row
                        [ width fill
                        , height fill
                        , spacing spacingSize
                        ]
                        (List.indexedMap
                            (\indexY _ ->
                                let
                                    id =
                                        (indexX + 1)
                                            * (indexY + 1)
                                in
                                el
                                    [ width <| fill
                                    , height <| fill
                                    , Background.color <| rgb 0 0.8 0
                                    , inFront <|
                                        row
                                            [ spacing 5
                                            , Font.color primaryColor
                                            , Background.color <| rgba 1 1 1 0.5
                                            , Border.rounded 10
                                            , padding 5
                                            , moveDown 10
                                            , moveRight 10
                                            ]
                                            [ el
                                                [ Font.size 36
                                                , Font.bold
                                                ]
                                                (text <| String.fromInt id)
                                            , Input.button [] { label = el [] <| html <| Material.Icons.open_in_full 30 Material.Icons.Types.Inherit, onPress = Just <| MsgChangeModality <| ModalityFullscreen id }
                                            , el [] <| html <| Material.Icons.open_in_new 30 Material.Icons.Types.Inherit
                                            , el [] <| html <| Material.Icons.edit 30 Material.Icons.Types.Inherit
                                            , el [] <| html <| Material.Icons.settings 30 Material.Icons.Types.Inherit
                                            , el [ Font.size 28 ] <| text <| Maybe.withDefault "" <| Dict.get (String.fromInt id) model.permanentState.attendees
                                            ]
                                    ]
                                <|
                                    html <|
                                        Html.iframe
                                            [ Html.Attributes.style "border" "0"
                                            , Html.Attributes.style "height" "100%"
                                            , Html.Attributes.style "background" "#4a0"

                                            -- , Html.Attributes.src "https://07-elm-boot.lucamug.repl.co/"
                                            , Html.Attributes.src "https://example.com/"

                                            -- , Html.Attributes.src "https://repubblica.it/"
                                            ]
                                            []
                            )
                            (List.repeat x 0)
                        )
                )
                (List.repeat y 0)
            )



-- column []
--     ([ text "Hi"
--      , Input.text []
--         { label = Input.labelHidden ""
--         , onChange = MsgChangeX
--         , placeholder = Nothing
--         , text = permanentState.x
--         }
--      , Input.text []
--         { label = Input.labelHidden ""
--         , onChange = MsgChangeY
--         , placeholder = Nothing
--         , text = permanentState.y
--         }
--      ]
--         ++ List.indexedMap
--             (\index _ ->
--                 Input.text []
--                     { label = Input.labelHidden ""
--                     , onChange = MsgChangeAttendee index
--                     , placeholder = Nothing
--                     , text = Maybe.withDefault "" <| List.Extra.getAt index permanentState.attendees
--                     }
--             )
--             (List.repeat (xy.x * xy.y) ())
--     )


permanentStateToXY : { a | x : String, y : String } -> { x : Int, y : Int }
permanentStateToXY args =
    { x = min 5 (Maybe.withDefault 1 <| String.toInt args.x)
    , y = min 4 (Maybe.withDefault 1 <| String.toInt args.y)
    }
