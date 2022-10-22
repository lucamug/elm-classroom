port module Main exposing (main)

import Base64
import Browser
import Browser.Events
import Codec
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes
import Json.Decode
import List.Extra
import Material.Icons
import Material.Icons.Types
import String.Extra
import Url



-- https://replit.com/@lucamug/07-elm-boot
-- https://replit.com/join/nscbimakhl-lucamug
--
-- https://replit.com/join/cdxnggkyhb-lucamug
-- https://01.lucamug.repl.co/


port pushUrl : { url : String, sendItBack : Bool } -> Cmd msg


port onUrlChange : (String -> msg) -> Sub msg


port changeMeta : { querySelector : String, fieldName : String, content : String } -> Cmd msg


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : a -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onKeyDown (Json.Decode.map MsgKeypress (Json.Decode.field "key" Json.Decode.string))
        , onUrlChange MsgUrlChanged
        ]


type alias Flags =
    { locationHref : String
    }


type alias Model =
    { permanentState : PermanentState
    , modality : Modality
    , locationHref : String
    }


type alias PermanentState =
    { x : String
    , y : String
    , attendees : Dict.Dict String String
    , invitations : Dict.Dict String String
    , invitationTemplate : String
    , previews : Dict.Dict String String
    , previewTemplate : String
    , title : String
    }


locationHrefToPermanentState : String -> PermanentState
locationHrefToPermanentState locationHref =
    locationHref
        |> Url.fromString
        |> Maybe.map .path
        |> Maybe.withDefault ""
        |> String.dropLeft 1
        |> Base64.decode
        |> Result.withDefault ""
        |> Codec.decodeString codecPermanentState
        |> Result.withDefault initPermanentState


initSize : { x : number, y : number1 }
initSize =
    { x = 4, y = 3 }


initDict : String -> Dict.Dict String String
initDict prefix =
    Dict.fromList <| List.indexedMap (\index _ -> ( String.fromInt index, prefix ++ String.fromInt index )) (List.repeat ((initSize.x * initSize.y) + 1) ())


titleText : String
titleText =
    "Elm Classroom"


initPermanentState : PermanentState
initPermanentState =
    { x = String.fromInt initSize.x
    , y = String.fromInt initSize.y
    , attendees = initDict "Attendee "
    , invitations = initDict "invitation_"
    , invitationTemplate = "https://example.com/?invitation={id}"
    , previews = initDict "preview_"
    , previewTemplate = "https://example.com/?preview={id}"
    , title = titleText
    }


initPermanentStateEmpty : PermanentState
initPermanentStateEmpty =
    { x = String.fromInt initSize.x
    , y = String.fromInt initSize.y
    , attendees = Dict.empty
    , invitations = Dict.empty
    , invitationTemplate = ""
    , previews = Dict.empty
    , previewTemplate = ""
    , title = titleText
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        permanentState : PermanentState
        permanentState =
            locationHrefToPermanentState flags.locationHref
    in
    ( { permanentState = permanentState
      , modality = ModalityNormal
      , locationHref = flags.locationHref
      }
    , changeMeta { content = permanentState.title, fieldName = "innerHTML", querySelector = "title" }
    )


type Modality
    = ModalityNormal
    | ModalitySettings
    | ModalityEditing
    | ModalityFullscreen Int


codecPermanentState : Codec.Codec PermanentState
codecPermanentState =
    Codec.object
        (\x y attendees invitations invitationTemplate previews previewTemplate title ->
            { x = x
            , y = y
            , attendees = attendees
            , invitations = invitations
            , invitationTemplate = invitationTemplate
            , previews = previews
            , previewTemplate = previewTemplate
            , title = title
            }
        )
        |> Codec.field "x" .x Codec.string
        |> Codec.field "y" .y Codec.string
        |> Codec.field "a" .attendees (Codec.dict Codec.string)
        |> Codec.field "b" .invitations (Codec.dict Codec.string)
        |> Codec.field "c" .invitationTemplate Codec.string
        |> Codec.field "d" .previews (Codec.dict Codec.string)
        |> Codec.field "e" .previewTemplate Codec.string
        |> Codec.field "f" .title Codec.string
        |> Codec.buildObject


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MsgDeleteAll ->
            ( { model | permanentState = initPermanentStateEmpty }
            , Cmd.batch
                [ pushUrl { sendItBack = False, url = buidlUrl model.locationHref initPermanentStateEmpty }
                , changeMeta { content = initPermanentStateEmpty.title, fieldName = "innerHTML", querySelector = "title" }
                ]
            )

        MsgKeypress char ->
            if char == "Escape" then
                ( { model | modality = ModalityNormal }, Cmd.none )

            else
                ( model, Cmd.none )

        MsgUrlChanged locationHref ->
            ( { model | permanentState = locationHrefToPermanentState locationHref }, Cmd.none )

        MsgChangeValue valueType id value ->
            let
                permanentState : PermanentState
                permanentState =
                    model.permanentState

                newPermanentState : PermanentState
                newPermanentState =
                    case valueType of
                        ValueX ->
                            { permanentState | x = value }

                        ValueY ->
                            { permanentState | y = value }

                        ValueAttendee ->
                            { permanentState | attendees = Dict.insert id value permanentState.attendees }

                        ValueInvitation ->
                            { permanentState | invitations = Dict.insert id value permanentState.invitations }

                        ValuePreview ->
                            { permanentState | previews = Dict.insert id value permanentState.previews }

                        ValueInvitationTemplate ->
                            { permanentState | invitationTemplate = value }

                        ValuePreviewTemplate ->
                            { permanentState | previewTemplate = value }

                        ValueTitle ->
                            { permanentState | title = value }
            in
            ( { model | permanentState = newPermanentState }
            , Cmd.batch
                [ pushUrl { sendItBack = False, url = buidlUrl model.locationHref newPermanentState }
                , changeMeta { content = newPermanentState.title, fieldName = "innerHTML", querySelector = "title" }
                ]
            )

        MsgChangeModality modality ->
            ( { model | modality = modality }, Cmd.none )


buidlUrl : String -> PermanentState -> String
buidlUrl locationHref permanentState =
    locationHref
        |> Url.fromString
        |> Maybe.map (\url -> { url | path = "/" ++ Base64.encode (Codec.encodeToString 0 codecPermanentState permanentState) })
        |> Maybe.map Url.toString
        |> Maybe.withDefault locationHref


type Msg
    = MsgChangeValue Value String String
    | MsgChangeModality Modality
    | MsgUrlChanged String
    | MsgKeypress String
    | MsgDeleteAll


type Value
    = ValueAttendee
    | ValueInvitation
    | ValuePreview
    | ValueX
    | ValueY
    | ValueInvitationTemplate
    | ValuePreviewTemplate
    | ValueTitle


primaryColor : Color
primaryColor =
    rgb 0.8 0.3 0


menuAttrs : List (Attribute msg)
menuAttrs =
    [ spacing 8
    , Font.color primaryColor
    , Background.color <| rgba 1 1 1 0.5
    , Border.rounded 10
    , Border.width 1
    , padding 5
    , moveDown 10
    ]


iconSize : Int
iconSize =
    25


buttonEdit : Element Msg
buttonEdit =
    Input.button [] { label = el [] <| html <| Material.Icons.edit iconSize Material.Icons.Types.Inherit, onPress = Just <| MsgChangeModality <| ModalityEditing }


buttonInvitation : Model -> Int -> Element msg
buttonInvitation model id =
    newTabLink [] { url = urlInvitation model id, label = el [] <| html <| Material.Icons.open_in_new iconSize Material.Icons.Types.Inherit }


buttonSettings : Element Msg
buttonSettings =
    Input.button [] { label = el [] <| html <| Material.Icons.settings iconSize Material.Icons.Types.Inherit, onPress = Just <| MsgChangeModality <| ModalitySettings }


buttonSave : Element Msg
buttonSave =
    Input.button [] { label = el [] <| html <| Material.Icons.save iconSize Material.Icons.Types.Inherit, onPress = Just <| MsgChangeModality <| ModalityNormal }


iconMenuLeft : Model -> Maybe Int -> Attribute Msg
iconMenuLeft model maybeId =
    inFront <|
        row
            (menuAttrs ++ [ moveRight 10 ])
            (case maybeId of
                Nothing ->
                    []

                Just id ->
                    []
                        ++ [ el [ Font.size 24, Font.bold, paddingEach { top = 0, right = 10, bottom = 0, left = 10 } ] (text <| String.fromInt id) ]
                        ++ (case model.modality of
                                ModalityNormal ->
                                    [ buttonEdit
                                    , buttonInvitation model id
                                    , buttonSettings
                                    ]

                                ModalityEditing ->
                                    [ buttonSave ]

                                ModalityFullscreen id_ ->
                                    [ buttonEdit
                                    , buttonInvitation model id
                                    , buttonSettings
                                    ]

                                ModalitySettings ->
                                    [ buttonSave ]
                           )
                        ++ [ el [ Font.size 20, moveDown 3 ] <| text <| getValue id model.permanentState.attendees ]
            )


getValue : Int -> Dict.Dict String String -> String
getValue id values =
    Maybe.withDefault "" <| Dict.get (String.fromInt id) values


iconMenuRight : Model -> Maybe Int -> Attribute Msg
iconMenuRight model maybeId =
    inFront <|
        row
            (menuAttrs ++ [ alignRight, moveLeft 10 ])
            (case maybeId of
                Nothing ->
                    []

                Just id ->
                    []
                        ++ (case model.modality of
                                ModalityFullscreen int ->
                                    if int == id then
                                        [ Input.button [] { label = el [] <| html <| Material.Icons.close_fullscreen iconSize Material.Icons.Types.Inherit, onPress = Just <| MsgChangeModality <| ModalityNormal } ]

                                    else
                                        [ Input.button [] { label = el [] <| html <| Material.Icons.open_in_full iconSize Material.Icons.Types.Inherit, onPress = Just <| MsgChangeModality <| ModalityFullscreen id } ]

                                _ ->
                                    [ Input.button [] { label = el [] <| html <| Material.Icons.open_in_full iconSize Material.Icons.Types.Inherit, onPress = Just <| MsgChangeModality <| ModalityFullscreen id } ]
                           )
            )


attrsFullscreen : Model -> Maybe Int -> List (Attribute Msg)
attrsFullscreen model maybeId =
    [ htmlAttribute <| Html.Attributes.style "width" "87vw"
    , htmlAttribute <| Html.Attributes.style "height" "80vh"
    , centerX
    , centerY
    , Background.color <| rgb 1 1 1
    , Border.width spacingSize
    , Border.color primaryColor
    , iconMenuLeft model maybeId
    , iconMenuRight model maybeId
    ]


viewFullscreen : Model -> Int -> Element Msg
viewFullscreen model id =
    el (attrsFullscreen model (Just id)) (iframe model id)


iframe : Model -> Int -> Element msg
iframe model id =
    html <|
        Html.iframe
            [ Html.Attributes.style "border" "0"
            , Html.Attributes.style "height" "100%"
            , Html.Attributes.src <| urlPreview model id
            ]
            []


spacingSize : Int
spacingSize =
    10


underCover : Attribute Msg
underCover =
    inFront <|
        Input.button
            [ width fill
            , height fill
            , padding 100
            , Background.color <| rgba 0 0 0 0.4
            ]
            { label = text ""
            , onPress = Just <| MsgChangeModality ModalityNormal
            }


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
        ([ Font.size 16 ]
            ++ (case model.modality of
                    ModalityFullscreen _ ->
                        [ underCover ]

                    ModalitySettings ->
                        [ underCover ]

                    _ ->
                        []
               )
            ++ (case model.modality of
                    ModalityFullscreen id ->
                        [ inFront <| viewFullscreen model id ]

                    ModalitySettings ->
                        [ inFront <| viewEditing model ]

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
                                viewPreview model ((indexX + 1) + (indexY * x))
                            )
                            (List.repeat x 0)
                        )
                )
                (List.repeat y 0)
            )


viewPreview : Model -> Int -> Element Msg
viewPreview model id =
    el
        ([ width <| fill
         , height <| fill
         , Background.color <| rgb 0.8 0.8 0.8
         ]
            ++ (case model.modality of
                    ModalityEditing ->
                        [ inFront <|
                            column
                                [ width fill
                                , height fill
                                , Background.color <| rgba 0 0 0 0.4
                                , paddingEach { top = 55, right = 10, bottom = 0, left = 10 }
                                , spacing 10
                                ]
                            <|
                                [ inputField { existingData = model.permanentState.attendees, id = id, label = "Attendee", valueType = ValueAttendee }
                                , inputField { existingData = model.permanentState.invitations, id = id, label = "Invitation", valueType = ValueInvitation }
                                , inputField { existingData = model.permanentState.previews, id = id, label = "Preview", valueType = ValuePreview }
                                ]
                        ]

                    _ ->
                        []
               )
            ++ [ iconMenuLeft model (Just id) ]
            ++ [ iconMenuRight model (Just id) ]
        )
        (iframe model id)


urlPreview : Model -> Int -> String
urlPreview model id =
    String.replace
        "{id}"
        (getValue id model.permanentState.previews)
        model.permanentState.previewTemplate


urlInvitation : Model -> Int -> String
urlInvitation model id =
    String.replace
        "{id}"
        (getValue id model.permanentState.invitations)
        model.permanentState.invitationTemplate


viewEditing : Model -> Element Msg
viewEditing model =
    el
        (attrsFullscreen model Nothing)
        (column [ width (fill |> maximum 800), centerX, padding 50, spacing 20, scrollbars ]
            [ paragraph [ Font.center, Font.size 30 ] [ text "Classroom" ]
            , text ""
            , paragraph []
                [ el [ Font.bold ] <| text "Classroom"
                , text " helps to setup a multiscreen view of the attendees in a workshop."
                ]
            , paragraph [] [ text "Use as Title the name of the workshop and other notes. It will be used in the title of the page and it will appear in the Browser Bookmark and Browser History." ]
            , inputField2 { id = 0, label = "Title", valueType = ValueTitle } model.permanentState.title
            , paragraph [ Font.bold ] [ text "Size" ]
            , paragraph [] [ text "You can specify the quantity of attendees. The largest supported size is 6 by 5." ]
            , row [ width fill ]
                [ inputField2 { id = 0, label = "X", valueType = ValueX } model.permanentState.x
                , inputField2 { id = 0, label = "Y", valueType = ValueY } model.permanentState.y
                ]
            , paragraph [ Font.bold ] [ text "Templates" ]
            , paragraph [] [ text "Templates can be used both for the Invitation URL and the Preview URL." ]
            , inputField2 { id = 0, label = "Invitation", valueType = ValueInvitationTemplate } model.permanentState.invitationTemplate
            , inputField2 { id = 0, label = "Preview", valueType = ValuePreviewTemplate } model.permanentState.previewTemplate
            , column
                [ Border.width 1
                , Border.rounded 10
                , Border.color <| rgb 0.8 0 0
                , Background.color <| rgb 1 0.9 0.9
                , Font.color <| rgb 0.8 0 0
                , padding 20
                , width fill
                , spacing 20
                ]
                [ paragraph [ Font.bold ] [ text "Dangerous area" ]
                , Input.button
                    [ padding 10
                    , Border.rounded 10
                    , Font.color <| rgb 1 1 1
                    , Background.color <| rgb 0.8 0 0
                    ]
                    { label = text "Reset All", onPress = Just MsgDeleteAll }
                ]
            ]
        )


inputField2 : { a | id : Int, label : String, valueType : Value } -> String -> Element Msg
inputField2 args textValue =
    row
        [ Background.color <| rgba 1 1 1 0.8
        , padding 5
        , spacing 10
        , Border.rounded 10
        , width fill
        ]
        [ el [ width <| px 80, Font.alignRight ] <| text args.label
        , Input.text [ Border.rounded 6, padding 10 ]
            { onChange = MsgChangeValue args.valueType (String.fromInt args.id)
            , text = textValue
            , placeholder = Nothing
            , label = Input.labelHidden args.label
            }
        ]


inputField : { existingData : Dict.Dict String String, id : Int, label : String, valueType : Value } -> Element Msg
inputField args =
    inputField2 args (getValue args.id args.existingData)


permanentStateToXY : { a | x : String, y : String } -> { x : Int, y : Int }
permanentStateToXY args =
    { x = min 6 (Maybe.withDefault 1 <| String.toInt args.x)
    , y = min 5 (Maybe.withDefault 1 <| String.toInt args.y)
    }
