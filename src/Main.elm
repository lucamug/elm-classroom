module Main exposing (main)

import Base64
import Browser
import Codec
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes
import List.Extra
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
    }


type alias PermanentState =
    { x : String
    , y : String
    , attendees : List String
    }


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
        |> Codec.field "a" .attendees (Codec.list Codec.string)
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

        MsgChangeAttendee int string ->
            ( { model
                | permanentState =
                    { permanentState
                        | attendees = List.Extra.setAt int string permanentState.attendees
                    }
              }
            , Cmd.none
            )


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        initPermanentState : PermanentState
        initPermanentState =
            { x = "4"
            , y = "3"
            , attendees = [ "Mark", "John" ]
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
    ( { permanentState = permanentState }
    , Cmd.none
    )


type Msg
    = MsgChangeX String
    | MsgChangeY String
    | MsgChangeAttendee Int String


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
    layout [] <|
        column
            [ width fill
            , height fill
            , Background.color <| rgb 0 0.3 0
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
                                el
                                    [ width <| fill
                                    , height <| fill
                                    , Background.color <| rgb 0 0.8 0
                                    , inFront <|
                                        el
                                            [ Font.size 40
                                            , Background.color <| rgba 0 0 0 0.5
                                            , paddingEach { top = 8, right = 12, bottom = 8, left = 12 }
                                            , moveRight 10
                                            , moveDown 10
                                            , Border.rounded 10
                                            , Font.bold
                                            , Font.color <| rgb 1 1 1
                                            ]
                                            (text <|
                                                String.fromInt <|
                                                    (indexX + 1)
                                                        * (indexY + 1)
                                            )
                                    ]
                                <|
                                    html <|
                                        Html.iframe
                                            [ Html.Attributes.style "border" "0"
                                            , Html.Attributes.style "height" "100%"
                                            , Html.Attributes.style "background" "#4a0"
                                            , Html.Attributes.src "https://repubblica.it/"
                                            ]
                                            []
                            )
                            (List.repeat x 0)
                        )
                )
                (List.repeat x 0)
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
