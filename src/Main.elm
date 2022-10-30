port module Main exposing (Env, Flags, Modality, Model, Msg, PermanentState, main)

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
import Material.Icons
import Material.Icons.Types
import Url



-- Icons:
-- https://fonts.google.com/icons?selected=Material+Icons


port pushUrl : { url : String, sendItBack : Bool } -> Cmd msg


port onUrlChange : (String -> msg) -> Sub msg


port changeMeta : { querySelector : String, fieldName : String, content : String } -> Cmd msg


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


subscriptions : a -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onKeyDown (Json.Decode.map MsgKeypress (Json.Decode.field "key" Json.Decode.string))
        , onUrlChange MsgUrlChanged
        ]


type alias Env =
    { commitHash : String }


type alias Flags =
    { env : Env, locationHref : String }


type alias Model =
    { env : Env
    , locationHref : String
    , modality : Modality
    , permanentState : PermanentState
    }


type alias PermanentState =
    { attendees : Dict.Dict String String
    , invitationTemplate : String
    , invitations : Dict.Dict String String
    , previewTemplate : String
    , title : String
    , userId : String
    , workspaceTemplate : String
    , workspaces : Dict.Dict String String
    , x : String
    , y : String
    }


locationHrefToPermanentState : String -> PermanentState
locationHrefToPermanentState locationHref =
    locationHref
        |> Url.fromString
        |> Maybe.andThen .query
        |> Maybe.withDefault ""
        |> Base64.decode
        |> Result.withDefault ""
        |> Codec.decodeString codecPermanentState
        |> Result.withDefault initPermanentStateExample


initSize : { x : number, y : number1 }
initSize =
    { x = 5, y = 3 }


initDict : String -> Dict.Dict String String
initDict prefix =
    Dict.fromList <| List.indexedMap (\index _ -> ( String.fromInt index, prefix ++ String.fromInt index )) (List.repeat ((initSize.x * initSize.y) + 1) ())


titleText : String
titleText =
    "Classroom"


initPermanentStateExample : PermanentState
initPermanentStateExample =
    { attendees = initDict "Attendee "
    , invitationTemplate = "https://example.com/?invitation={id}&userid={userId}"
    , invitations = initDict "invitation_"
    , previewTemplate = "https://example.com/?preview={id}&userid={userId}"
    , title = titleText
    , userId = "exampleId"
    , workspaceTemplate = "https://example.com/?preview={id}&userid={userId}"
    , workspaces = initDict "workspaces_"
    , x = String.fromInt initSize.x
    , y = String.fromInt initSize.y
    }


initPermanentStateReplit : PermanentState
initPermanentStateReplit =
    { attendees =
        Dict.fromList
            [ ( "1", "Main" )
            , ( "2", "Kyosuke" )
            , ( "3", "Jin" )
            , ( "4", "Md" )
            , ( "5", "Skandar" )
            , ( "6", "Eric" )
            , ( "7", "Takanori" )
            , ( "8", "Tamer" )
            , ( "9", "May" )
            , ( "10", "Jose" )
            , ( "11", "Anshul" )
            , ( "12", "Scott" )
            , ( "13", "Dinesh" )
            , ( "14", "Extra 1" )
            , ( "15", "Extra 2" )
            ]
    , invitationTemplate = "https://replit.com/join/{id}-{userId}"
    , invitations =
        Dict.fromList
            [ ( "1", "oqqeljgfln" )
            , ( "2", "ziibbtpuwy" )
            , ( "3", "blszhgxucp" )
            , ( "4", "ycjqbmzrvl" )
            , ( "5", "rbkrqvoedm" )
            , ( "6", "yqbzsjiouz" )
            , ( "7", "ywasebvixw" )
            , ( "8", "" )
            , ( "9", "" )
            , ( "10", "wgybsatrfv" )
            , ( "11", "" )
            , ( "12", "" )
            , ( "13", "" )
            , ( "14", "" )
            , ( "15", "" )
            ]
    , previewTemplate = "https://{id}.{userId}.repl.co/"
    , title = "Functional Programming with Elm"
    , userId = "lucamug"
    , workspaceTemplate = "https://replit.com/@{userId}/{id}#src/Main.elm    "
    , workspaces =
        Dict.fromList
            [ ( "1", "01" )
            , ( "2", "02" )
            , ( "3", "03" )
            , ( "4", "04" )
            , ( "5", "05" )
            , ( "6", "06" )
            , ( "7", "07" )
            , ( "8", "08" )
            , ( "9", "09" )
            , ( "10", "10" )
            , ( "11", "11" )
            , ( "12", "12" )
            , ( "13", "13" )
            , ( "14", "14" )
            , ( "15", "15" )
            ]
    , x = String.fromInt initSize.x
    , y = String.fromInt initSize.y
    }


initPermanentStateEmpty : PermanentState
initPermanentStateEmpty =
    { attendees = Dict.empty
    , invitationTemplate = ""
    , invitations = Dict.empty
    , previewTemplate = ""
    , title = titleText
    , userId = ""
    , workspaceTemplate = ""
    , workspaces = Dict.empty
    , x = String.fromInt initSize.x
    , y = String.fromInt initSize.y
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        permanentState : PermanentState
        permanentState =
            locationHrefToPermanentState flags.locationHref
    in
    ( { env = flags.env
      , locationHref = flags.locationHref
      , modality = ModalityNormal
      , permanentState = permanentState
      }
    , changeMeta { content = permanentState.title, fieldName = "innerHTML", querySelector = "title" }
    )


type Modality
    = ModalityEditing
    | ModalityFullscreen Int
    | ModalityNormal
    | ModalitySettings


codecPermanentState : Codec.Codec PermanentState
codecPermanentState =
    Codec.object
        (\x y attendees invitations invitationTemplate workspaces previewTemplate workspaceTemplate userId title ->
            { attendees = attendees
            , invitationTemplate = invitationTemplate
            , invitations = invitations
            , previewTemplate = previewTemplate
            , title = title
            , userId = userId
            , workspaceTemplate = workspaceTemplate
            , workspaces = workspaces
            , x = x
            , y = y
            }
        )
        |> Codec.field "x" .x Codec.string
        |> Codec.field "y" .y Codec.string
        |> Codec.field "a" .attendees (Codec.dict Codec.string)
        |> Codec.field "b" .invitations (Codec.dict Codec.string)
        |> Codec.field "c" .invitationTemplate Codec.string
        |> Codec.field "d" .workspaces (Codec.dict Codec.string)
        |> Codec.field "e" .previewTemplate Codec.string
        |> Codec.field "f" .workspaceTemplate Codec.string
        |> Codec.field "g" .userId Codec.string
        |> Codec.field "h" .title Codec.string
        |> Codec.buildObject


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MsgChangeModality modality ->
            ( { model | modality = modality }, Cmd.none )

        MsgChangeValue valueType id value ->
            let
                permanentState : PermanentState
                permanentState =
                    model.permanentState

                newPermanentState : PermanentState
                newPermanentState =
                    case valueType of
                        ValueAttendee ->
                            { permanentState | attendees = Dict.insert id value permanentState.attendees }

                        ValueInvitation ->
                            { permanentState | invitations = Dict.insert id value permanentState.invitations }

                        ValueInvitationTemplate ->
                            { permanentState | invitationTemplate = value }

                        ValuePreviewTemplate ->
                            { permanentState | previewTemplate = value }

                        ValueTitle ->
                            { permanentState | title = value }

                        ValueUserId ->
                            { permanentState | userId = value }

                        ValueWorkspace ->
                            { permanentState | workspaces = Dict.insert id value permanentState.workspaces }

                        ValueWorkspaceTemplate ->
                            { permanentState | workspaceTemplate = value }

                        ValueX ->
                            { permanentState | x = value }

                        ValueY ->
                            { permanentState | y = value }
            in
            ( { model | permanentState = newPermanentState }
            , Cmd.batch
                [ pushUrl { sendItBack = False, url = buidlUrl model.locationHref newPermanentState }
                , changeMeta { content = newPermanentState.title, fieldName = "innerHTML", querySelector = "title" }
                ]
            )

        MsgKeypress char ->
            if char == "Escape" then
                ( { model | modality = ModalityNormal }, Cmd.none )

            else
                ( model, Cmd.none )

        MsgLoad permanentsState ->
            ( { model | permanentState = permanentsState }
            , Cmd.batch
                [ pushUrl { sendItBack = False, url = buidlUrl model.locationHref permanentsState }
                , changeMeta { content = permanentsState.title, fieldName = "innerHTML", querySelector = "title" }
                ]
            )

        MsgUrlChanged locationHref ->
            ( { model | permanentState = locationHrefToPermanentState locationHref }, Cmd.none )


buidlUrl : String -> PermanentState -> String
buidlUrl locationHref permanentState =
    locationHref
        |> Url.fromString
        |> Maybe.map (\url -> { url | query = Just <| Base64.encode (Codec.encodeToString 0 codecPermanentState permanentState) })
        |> Maybe.map Url.toString
        |> Maybe.withDefault locationHref


type Msg
    = MsgChangeModality Modality
    | MsgChangeValue Value String String
    | MsgKeypress String
    | MsgLoad PermanentState
    | MsgUrlChanged String


type Value
    = ValueAttendee
    | ValueInvitation
    | ValueInvitationTemplate
    | ValuePreviewTemplate
    | ValueTitle
    | ValueUserId
    | ValueWorkspace
    | ValueWorkspaceTemplate
    | ValueX
    | ValueY


primaryColor : Color
primaryColor =
    rgb 0.8 0.3 0


menuAttrs : List (Attribute msg)
menuAttrs =
    [ spacing 8
    , Font.color primaryColor
    , Background.color <| rgba 1 1 1 0.8
    , Border.rounded 10
    , Border.width 1
    , paddingEach { bottom = 5, left = 10, right = 10, top = 5 }
    ]


iconSize : Int
iconSize =
    25


buttonEdit : Element Msg
buttonEdit =
    Input.button [ htmlAttribute <| Html.Attributes.title "Edit" ] { label = el [] <| html <| Material.Icons.edit iconSize Material.Icons.Types.Inherit, onPress = Just <| MsgChangeModality <| ModalityEditing }


buttonInvitation : Model -> Int -> Element msg
buttonInvitation model id =
    newTabLink [ htmlAttribute <| Html.Attributes.title "Open Workspace by Invitation" ] { label = el [] <| html <| Material.Icons.mail iconSize Material.Icons.Types.Inherit, url = urlInvitation model id }


buttonWorkspace : Model -> Int -> Element msg
buttonWorkspace model id =
    newTabLink [ htmlAttribute <| Html.Attributes.title "Open Workspace" ] { label = el [] <| html <| Material.Icons.open_in_new iconSize Material.Icons.Types.Inherit, url = urlWorkspace model id }


buttonSettings : Element Msg
buttonSettings =
    Input.button [ htmlAttribute <| Html.Attributes.title "Settings" ] { label = el [] <| html <| Material.Icons.settings iconSize Material.Icons.Types.Inherit, onPress = Just <| MsgChangeModality <| ModalitySettings }


buttonSave : Element Msg
buttonSave =
    Input.button [ htmlAttribute <| Html.Attributes.title "Save" ] { label = el [] <| html <| Material.Icons.save iconSize Material.Icons.Types.Inherit, onPress = Just <| MsgChangeModality <| ModalityNormal }


buttonFullScreen : Int -> Element Msg
buttonFullScreen id =
    Input.button [ htmlAttribute <| Html.Attributes.title "Full screen" ] { label = el [] <| html <| Material.Icons.open_in_full iconSize Material.Icons.Types.Inherit, onPress = Just <| MsgChangeModality <| ModalityFullscreen id }


buttonFullScreenClose : Element Msg
buttonFullScreenClose =
    Input.button [] { label = el [ htmlAttribute <| Html.Attributes.title "Close full screen" ] <| html <| Material.Icons.close_fullscreen iconSize Material.Icons.Types.Inherit, onPress = Just <| MsgChangeModality <| ModalityNormal }


iconMenuLeft : Model -> Maybe Int -> Attribute Msg
iconMenuLeft model maybeId =
    inFront <|
        row
            (menuAttrs ++ [ moveRight 10, moveDown 10 ])
            (case maybeId of
                Just id ->
                    []
                        ++ [ el [ Font.size 24, Font.bold, moveDown 1 ] (text <| String.fromInt id) ]
                        ++ (case model.modality of
                                ModalityEditing ->
                                    [ buttonSave ]

                                ModalityFullscreen _ ->
                                    [ buttonInvitation model id
                                    , buttonWorkspace model id
                                    ]

                                ModalityNormal ->
                                    [ buttonInvitation model id
                                    , buttonWorkspace model id
                                    ]

                                ModalitySettings ->
                                    []
                           )
                        ++ [ el [ Font.size 20, moveDown 1.5 ] <| text <| getValue id model.permanentState.attendees ]

                Nothing ->
                    [ el [ Font.size 24 ] <| text "Settings"
                    , buttonSave
                    ]
            )


iconMenuRight : Model -> Maybe Int -> Attribute Msg
iconMenuRight model maybeId =
    inFront <|
        row
            (menuAttrs ++ [ alignLeft, alignBottom, moveUp 10, moveRight 10 ])
            (case maybeId of
                Just id ->
                    []
                        ++ (case model.modality of
                                ModalityFullscreen int ->
                                    if int == id then
                                        [ buttonFullScreenClose ]

                                    else
                                        [ buttonEdit
                                        , buttonSettings
                                        , buttonFullScreen id
                                        ]

                                _ ->
                                    [ buttonEdit
                                    , buttonSettings
                                    , buttonFullScreen id
                                    ]
                           )

                Nothing ->
                    [ buttonSave ]
            )


getValue : Int -> Dict.Dict String String -> String
getValue id values =
    Maybe.withDefault "" <| Dict.get (String.fromInt id) values


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
                                , paddingEach { bottom = 0, left = 10, right = 10, top = 55 }
                                , spacing 10
                                ]
                            <|
                                [ inputField { existingData = model.permanentState.attendees, id = id, label = "Attendee", valueType = ValueAttendee }
                                , inputField { existingData = model.permanentState.invitations, id = id, label = "Invitation", valueType = ValueInvitation }
                                , inputField { existingData = model.permanentState.workspaces, id = id, label = "Workspace", valueType = ValueWorkspace }
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
    urlBuilder { id = id, template = model.permanentState.previewTemplate, userId = model.permanentState.userId, values = model.permanentState.workspaces }


urlInvitation : Model -> Int -> String
urlInvitation model id =
    urlBuilder { id = id, template = model.permanentState.invitationTemplate, userId = model.permanentState.userId, values = model.permanentState.invitations }


urlWorkspace : Model -> Int -> String
urlWorkspace model id =
    urlBuilder { id = id, template = model.permanentState.workspaceTemplate, userId = model.permanentState.userId, values = model.permanentState.workspaces }


urlBuilder : { id : Int, template : String, userId : String, values : Dict.Dict String String } -> String
urlBuilder { id, template, userId, values } =
    template
        |> String.replace "{id}" (getValue id values)
        |> String.replace "{userId}" userId


viewEditing : Model -> Element Msg
viewEditing model =
    el
        (attrsFullscreen model Nothing)
    <|
        el [ width fill, scrollbars ] <|
            column [ width (fill |> maximum 800), centerX, padding 50, spacing 20 ]
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
                , inputField2 { id = 0, label = "User ID", valueType = ValueUserId } model.permanentState.userId
                , paragraph [ Font.bold ] [ text "Templates" ]
                , paragraph [] [ text "Templates can be used both for the Invitation URL and the Preview URL." ]
                , inputField2 { id = 0, label = "Invitation", valueType = ValueInvitationTemplate } model.permanentState.invitationTemplate
                , inputField2 { id = 0, label = "Preview", valueType = ValuePreviewTemplate } model.permanentState.previewTemplate
                , inputField2 { id = 0, label = "Workspace", valueType = ValueWorkspaceTemplate } model.permanentState.workspaceTemplate
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
                    , let
                        buttonAttrs : List (Attribute msg)
                        buttonAttrs =
                            [ padding 10
                            , Border.rounded 10
                            , Font.color <| rgb 1 1 1
                            , Background.color <| rgb 0.8 0 0
                            ]
                      in
                      row [ spacing 20 ]
                        [ Input.button buttonAttrs { label = text "Load Example", onPress = Just <| MsgLoad initPermanentStateExample }
                        , Input.button buttonAttrs { label = text "Load Empty", onPress = Just <| MsgLoad initPermanentStateEmpty }
                        , Input.button buttonAttrs { label = text "Load Replit", onPress = Just <| MsgLoad initPermanentStateReplit }
                        ]
                    ]
                , column
                    [ Font.center
                    , Font.size 14
                    , Font.color <| rgba 0 0 0 0.5
                    , spacing 10
                    , width fill
                    , paddingEach { bottom = 0, left = 0, right = 0, top = 60 }
                    , width fill
                    ]
                    [ paragraph [] [ text <| "Commit " ++ model.env.commitHash ]
                    , paragraph []
                        [ text "Carefully crafted with Elm and "
                        , el [ Font.color <| rgba 0.8 0.0 0.0 0.5, Font.size 16 ] <| text "♥"
                        ]
                    ]
                ]


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
            { label = Input.labelHidden args.label
            , onChange = MsgChangeValue args.valueType (String.fromInt args.id)
            , placeholder = Nothing
            , text = textValue
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
