module Select exposing (..)

import Html exposing (Html, div, input, i, li, ul, text, Attribute)
import Html.Attributes exposing (class, classList, value)
import Html.Events exposing (on, onClick, onInput, keyCode, onWithOptions)
import String exposing (contains)
import Json.Decode as Json


type Msg a
    = SelectValue ( String, a )
    | ClearValue
    | ToggleSelect
    | SearchValue String
    | EnteredValue
    | UpdateItems (List ( String, a )) (Maybe ( String, a ))


init : List ( String, a ) -> Maybe ( String, a ) -> Model a
init items defaultValue =
    { items = items
    , visibleItems = items
    , value = defaultValue
    , isOpen = False
    , searchValue = ""
    , canClear = True
    }


empty : Model a
empty =
    { items = []
    , visibleItems = []
    , value = Nothing
    , isOpen = False
    , searchValue = ""
    , canClear = False
    }


getValue : Model a -> Maybe a
getValue model =
    case model.value of
        Nothing ->
            Nothing

        Just ( _, x ) ->
            Just x


updateItems : Model a -> List ( String, a ) -> Maybe ( String, a ) -> Model a
updateItems model items selected =
    let
        model1 =
            { model | items = items, value = selected }
    in
        update (SearchValue model1.searchValue) model1


type alias Model a =
    { items : List ( String, a )
    , value : Maybe ( String, a )
    , visibleItems : List ( String, a )
    , isOpen : Bool
    , searchValue : String
    , canClear : Bool
    }


view : Model a -> Html (Msg a)
view model =
    (div
        [ class "select-container"
        , classList [ ( "select-open", model.isOpen ) ]
        ]
        [ selectedView model
        , searchView model
        , selectValuesView model
        ]
    )


searchView : Model a -> Html (Msg a)
searchView model =
    input [ onInput SearchValue, onEnterKey EnteredValue, value model.searchValue ] []


selectValuesView : Model a -> Html (Msg a)
selectValuesView model =
    let
        resultview =
            if (List.length model.visibleItems == 0) && not (model.searchValue == "") then
                div [ class "no-item" ] [ text "No match found" ]
            else
                ul
                    []
                    (List.map (\value -> selectValueView value model.value) model.visibleItems)
    in
        resultview


selectValueView : ( String, a ) -> Maybe ( String, a ) -> Html (Msg a)
selectValueView value selectedValue =
    let
        ( val, _ ) =
            value

        selectedVal =
            case selectedValue of
                Nothing ->
                    ""

                Just ( itemText, _ ) ->
                    itemText
    in
        li
            [ classList [ ( "item-selected", selectedVal == val ) ]
            , onClick (SelectValue value)
            ]
            [ text val ]


selectedView : Model a -> Html (Msg a)
selectedView model =
    let
        selectedText =
            case model.value of
                Nothing ->
                    ""

                Just ( itemText, _ ) ->
                    itemText
    in
        div [ class "select-selected-value", onClear ToggleSelect ]
            [ div
                [ class "select-value" ]
                [ text selectedText
                , div
                    [ class "select-clear"
                    , classList
                        [ ( "hide", canClear model.canClear selectedText )
                        ]
                    , onClear ClearValue
                    ]
                    [ text "x" ]
                ]
            , arrowView model.isOpen
            ]


onClear : Msg a -> Attribute (Msg a)
onClear msg =
    onWithOptions "click"
        { stopPropagation = True
        , preventDefault = True
        }
        (Json.succeed msg)


canClear : Bool -> String -> Bool
canClear allowed val =
    not (allowed && not (val == ""))


arrowView : Bool -> Html (Msg a)
arrowView open =
    let
        arrowClass =
            if open then
                "fa-angle-up"
            else
                "fa-angle-down"
    in
        div [ class "arrow-container" ]
            [ i [ class arrowClass ] [] ]


onEnterKey : Msg a -> Attribute (Msg a)
onEnterKey msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg
            else
                Json.fail "not ENTER"
    in
        on "keydown" (Json.andThen isEnter keyCode)


update : Msg a -> Model a -> Model a
update msg model =
    case msg of
        ToggleSelect ->
            { model | isOpen = (not model.isOpen) }

        SelectValue val ->
            { model | isOpen = False, value = Just val, searchValue = "", visibleItems = model.items }

        ClearValue ->
            { model | value = Nothing }

        SearchValue val ->
            let
                result =
                    if val == "" then
                        model.items
                    else
                        List.filter
                            (\( a, _ ) -> String.contains (String.toLower val) (String.toLower a))
                            model.items
            in
                ({ model | visibleItems = result, searchValue = val })

        EnteredValue ->
            let
                val =
                    List.head model.visibleItems

                result =
                    if val == Nothing then
                        model.value
                    else
                        val
            in
                { model | value = result, isOpen = False, searchValue = "", visibleItems = model.items }

        UpdateItems items selected ->
            let
                model1 =
                    { model | items = items, value = selected }
            in
                update (SearchValue model1.searchValue) model1
