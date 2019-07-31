module Main exposing (Model, Msg(..), Status(..), getData, init, main, personDecoder, subscriptions, update, view, viewData)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, decodeValue, dict, field, index, list, map2, string, succeed)
import Json.Decode.Pipeline as Pipeline exposing (..)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type Status
    = Failure
    | Loading
    | Success PersonData


type alias Model =
    { status : Status
    , query : String
    , dropdownSelection : String
    , dropdownValues : List String
    , data : PersonData
    , filteredData : PersonData
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { status = Loading, query = "skywalker", dropdownSelection = "Male", dropdownValues = [ "Male", "Female", "n/a" ], data = { results = [] }, filteredData = { results = [] } }, getData "anakin" )



-- UPDATE


type Msg
    = SearchAgain
    | GotResults (Result Http.Error PersonData)
    | UpdateQuery String
    | FilterResults String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        debugger =
            Debug.log "data" model.data.results
    in
    case msg of
        SearchAgain ->
            ( { model | status = Loading }, getData model.query )

        GotResults result ->
            case result of
                Ok data ->
                    ( { model | status = Success data, data = data, filteredData = { results = List.filter (\person -> person.gender == String.toLower model.dropdownSelection) model.data.results } }, Cmd.none )

                Err _ ->
                    ( { model | status = Failure }, Cmd.none )

        UpdateQuery query ->
            ( { model | query = query }, Cmd.none )

        FilterResults filterCriteria ->
            ( { model | filteredData = { results = List.filter (\person -> person.gender == String.toLower filterCriteria) model.data.results }, dropdownSelection = filterCriteria }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Databank Terminal" ]
        , viewData model
        ]


viewData : Model -> Html Msg
viewData model =
    case model.status of
        Failure ->
            div []
                [ text "Type search term here: "
                , input [ onInput UpdateQuery, value model.query ] []
                , br [] []
                , text "I could not retrieve results for some reason. "
                , button [ onClick SearchAgain ] [ text "Try Again!" ]
                ]

        Loading ->
            text "Loading..."

        Success data ->
            div []
                [ text "Type search term here: "
                , input [ onInput UpdateQuery, value model.query ] []
                , select [ onInput FilterResults ] <| List.map (\val -> createMenu val model.dropdownSelection) model.dropdownValues
                , br [] []
                , button [ onClick SearchAgain, style "display" "block" ] [ text "Search the databank" ]
                , br [] []
                , table []
                    [ thead []
                        [ tr []
                            [ th [ scope "col" ] [ text "Name" ]
                            , th [ scope "col" ] [ text "Gender" ]
                            , th [ scope "col" ] [ text "Birth Year" ]
                            ]
                        ]
                    , tbody [] <| List.map createRow model.filteredData.results
                    ]
                ]


createMenu : String -> String -> Html Msg
createMenu menuItem previousSelection =
    option [ value menuItem, selected (menuItem == previousSelection) ] [ text menuItem ]


createRow : Person -> Html Msg
createRow person =
    tr []
        [ td [] [ text person.name ]
        , td [] [ text person.gender ]
        , td [] [ text person.birthYear ]
        ]



-- HTTP


getData : String -> Cmd Msg
getData tag =
    Http.get
        { url = "https://swapi.co/api/people/?search=" ++ tag
        , expect = Http.expectJson GotResults peopleDecoder
        }


type alias Person =
    { birthYear : String, gender : String, name : String }


type alias PersonData =
    { results : List Person }


peopleDecoder : Decoder PersonData
peopleDecoder =
    succeed PersonData
        |> Pipeline.required "results" (list personDecoder)


personDecoder : Decoder Person
personDecoder =
    succeed Person
        |> Pipeline.required "birth_year" string
        |> Pipeline.required "gender" string
        |> Pipeline.required "name" string
