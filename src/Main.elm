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
    , invitations : Dict.Dict String String
    , frames : Dict.Dict String String
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        initPermanentState : PermanentState
        initPermanentState =
            { x = "4"
            , y = "3"
            , attendees = Dict.fromList [ ( "1", "Mark" ), ( "2", "John" ) ]
            , invitations = Dict.fromList [ ( "1", "Mark" ), ( "2", "John" ) ]
            , frames = Dict.fromList [ ( "1", "Mark" ), ( "2", "John" ) ]
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
        (\x y attendees invitations frames ->
            { x = x
            , y = y
            , attendees = attendees
            , invitations = invitations
            , frames = frames
            }
        )
        |> Codec.field "x" .x Codec.string
        |> Codec.field "y" .y Codec.string
        |> Codec.field "a" .attendees (Codec.dict Codec.string)
        |> Codec.field "i" .invitations (Codec.dict Codec.string)
        |> Codec.field "f" .frames (Codec.dict Codec.string)
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

        MsgChangeValue value id name ->
            case value of
                ValueAttendee ->
                    ( { model | permanentState = { permanentState | attendees = Dict.insert id name permanentState.attendees } }, Cmd.none )

                ValueInvitation ->
                    ( { model | permanentState = { permanentState | invitations = Dict.insert id name permanentState.invitations } }, Cmd.none )

                ValueFrame ->
                    ( { model | permanentState = { permanentState | frames = Dict.insert id name permanentState.frames } }, Cmd.none )

        MsgChangeModality modality ->
            ( { model | modality = modality }, Cmd.none )


type Msg
    = MsgChangeX String
    | MsgChangeY String
    | MsgChangeValue Value String String
    | MsgChangeModality Modality


type Value
    = ValueAttendee
    | ValueInvitation
    | ValueFrame


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


iconMenuLeft : Model -> Int -> Attribute Msg
iconMenuLeft model id_ =
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

                        ModalityEditing ->
                            [ Input.button [] { label = el [] <| html <| Material.Icons.save 30 Material.Icons.Types.Inherit, onPress = Just <| MsgChangeModality <| ModalityNormal } ]

                        ModalityFullscreen _ ->
                            [ Input.button [] { label = el [] <| html <| Material.Icons.open_in_full 30 Material.Icons.Types.Inherit, onPress = Just <| MsgChangeModality <| ModalityFullscreen id }
                            , Input.button [] { label = el [] <| html <| Material.Icons.edit 30 Material.Icons.Types.Inherit, onPress = Just <| MsgChangeModality <| ModalityEditing }
                            , el [] <| html <| Material.Icons.open_in_new 30 Material.Icons.Types.Inherit
                            , el [] <| html <| Material.Icons.settings 30 Material.Icons.Types.Inherit
                            ]

                        ModalitySettings ->
                            []
                   )
                ++ [ el [ Font.size 28, moveDown 3 ] <|
                        text <|
                            Maybe.withDefault "" <|
                                Dict.get (String.fromInt id) model.permanentState.attendees
                   ]
            )


viewFullscreen : Model -> Element Msg
viewFullscreen model =
    el
        [ htmlAttribute <| Html.Attributes.style "width" "87vw"
        , htmlAttribute <| Html.Attributes.style "height" "80vh"
        , centerX
        , centerY
        , Background.color <| rgb 1 1 1
        , Border.width spacingSize
        , Border.color primaryColor
        , iconMenuLeft model 0
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


spacingSize : Int
spacingSize =
    10


view : Model -> Html.Html Msg
view model =
    let
        permanentState : PermanentState
        permanentState =
            model.permanentState

        { x, y } =
            permanentStateToXY permanentState
    in
    layout
        ([]
            ++ (case model.modality of
                    ModalityFullscreen _ ->
                        [ inFront <|
                            Input.button
                                [ width fill
                                , height fill
                                , padding 100
                                , Background.color <| rgba 0 0 0 0.4
                                ]
                                { label = text ""
                                , onPress = Just <| MsgChangeModality ModalityNormal
                                }
                        ]

                    _ ->
                        []
               )
            ++ (case model.modality of
                    ModalityFullscreen int ->
                        [ inFront <| viewFullscreen model ]

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
                (\indexY _ ->
                    row
                        [ width fill
                        , height fill
                        , spacing spacingSize
                        ]
                        (List.indexedMap
                            (\indexX _ ->
                                viewFrame model ((indexX + 1) + (indexY * x))
                            )
                            (List.repeat x 0)
                        )
                )
                (List.repeat y 0)
            )


viewFrame : Model -> Int -> Element Msg
viewFrame model id =
    el
        ([ width <| fill
         , height <| fill
         , Background.color <| rgb 0 0.8 0
         ]
            ++ (case model.modality of
                    ModalityEditing ->
                        [ inFront <|
                            column
                                [ width fill
                                , height fill
                                , Background.color <| rgba 0 0 0 0.4
                                , paddingEach { top = 80, right = 10, bottom = 0, left = 10 }
                                , spacing 10
                                ]
                            <|
                                [ inputField { existingData = model.permanentState.attendees, id = id, label = "Attendee", valueType = ValueAttendee }
                                , inputField { existingData = model.permanentState.attendees, id = id, label = "Invitation", valueType = ValueInvitation }
                                , inputField { existingData = model.permanentState.attendees, id = id, label = "Frame", valueType = ValueFrame }
                                ]
                        ]

                    _ ->
                        []
               )
            ++ [ iconMenuLeft model id ]
        )
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


inputField :
    { existingData : Dict.Dict String String
    , id : Int
    , label : String
    , valueType : Value
    }
    -> Element Msg
inputField { label, valueType, existingData, id } =
    row
        [ Background.color <| rgba 1 1 1 0.8
        , padding 5
        , spacing 10
        , Border.rounded 10
        , width fill
        ]
        [ el [ width <| px 90, Font.alignRight ] <| text label
        , Input.text [ Border.rounded 6 ]
            { onChange = MsgChangeValue valueType (String.fromInt id)
            , text = Maybe.withDefault "" <| Dict.get (String.fromInt id) existingData
            , placeholder = Nothing
            , label = Input.labelHidden label
            }
        ]


permanentStateToXY : { a | x : String, y : String } -> { x : Int, y : Int }
permanentStateToXY args =
    { x = min 5 (Maybe.withDefault 1 <| String.toInt args.x)
    , y = min 4 (Maybe.withDefault 1 <| String.toInt args.y)
    }
