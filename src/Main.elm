port module Main exposing (..)

import Array exposing (fromList, get)
import Browser
import Html exposing (..)
import Html.Attributes exposing (class, type_)
import Html.Events exposing (onClick)
import Json.Decode as D
import Json.Encode as E



-- MAIN


main : Program E.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- PORTS


port newStep : String -> Cmd msg



-- MODEL


steps =
    fromList
        [ "Joy/Appreciation/Empowerment/Freedom/Love"
        , "Passion"
        , "Enthusiasm/Eagerness/Happiness"
        , "Positive Expectation/Belief"
        , "Optimism"
        , "Hopefulness"
        , "Contentment"
        , "Boredom"
        , "Pessimism"
        , "Frustration/Irritation/Impatience"
        , "Overwhelment"
        , "Disappointment"
        , "Doubt"
        , "Worry"
        , "Blame"
        , "Discouragement"
        , "Anger"
        , "Revenge"
        , "Hatred/Rage"
        , "Jealousy"
        , "Insecurity/Guilt/Unworthiness"
        , "Fear/Grief/Depression/Despair/Powerlessness"
        ]


sanitised step =
    Array.length steps |> min step |> max 1


type alias Model =
    Int


decoder : D.Decoder (Maybe Int)
decoder =
    D.map String.toInt D.string


init : E.Value -> ( Model, Cmd Msg )
init flags =
    ( case D.decodeValue decoder flags of
        Ok (Just step) ->
            sanitised step

        _ ->
            7
    , Cmd.none
    )



-- UPDATE


type Msg
    = NewStep Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        NewStep i ->
            let
                step =
                    sanitised i
            in
            ( step, newStep <| String.fromInt step )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container", class "text-center" ]
        [ div [ class "list-group" ]
            [ getMove model Up
            , viewStep model
            , getMove model Down
            ]
        ]


type UpOrDown
    = Up
    | Down


getMove : Int -> UpOrDown -> Html Msg
getMove currentStep upOrDown =
    let
        stepInt =
            case upOrDown of
                Up ->
                    currentStep - 1

                Down ->
                    currentStep + 1

        attrs =
            [ type_ "button", class "list-group-item", class "list-group-item-action" ]
    in
    case stepToStr stepInt of
        Just str ->
            case upOrDown of
                Up ->
                    button (attrs ++ [ onClick <| NewStep stepInt, class "text-success" ]) [ span [ class "float-start" ] [ text "⬆" ], text str ]

                Down ->
                    button (attrs ++ [ onClick <| NewStep stepInt, class "text-danger" ]) [ span [ class "float-start" ] [ text "⬇" ], text str ]

        Nothing ->
            button (attrs ++ [ class "disabled" ]) [ text "-" ]


viewStep : Int -> Html Msg
viewStep currentStep =
    (case stepToStr currentStep of
        Just str ->
            [ text <| String.fromInt currentStep ++ ": " ++ str ]

        Nothing ->
            [ text "..." ]
    )
        |> div [ class "list-group-item", class "active" ]


stepToStr : Int -> Maybe String
stepToStr number =
    get (number - 1) steps
