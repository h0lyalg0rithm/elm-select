module Select exposing (..)

{-| This library handles the logic for the dropdown. To use the library you have to pass in the
different options in the dropdown as a List of Tuple (String, Model)

The Model must be of the same type when in the list.

Example
[(1,{id=1, name="test"}),(2, {id=2, name="second"})]

-}

import Html exposing (Html, div, input, i, li, ul, text, Attribute)
import Html.Attributes exposing (class, classList, value)
import Html.Events exposing (on, onClick, onInput, keyCode, onWithOptions)
import String exposing (contains)
import Json.Decode as Json


{-| @docs Different Msg used for the internal working of the dropdown
-}
type Msg a
    = SelectValue ( String, a )
    | ClearValue
    | ToggleSelect
    | SearchValue String
    | EnteredValue
    | UpdateItems (List ( String, a )) (Maybe ( String, a ))


{-| @docs Initial model generated based on the List (String , a) Tuple provided by the user
-}
init : List ( String, a ) -> Maybe ( String, a ) -> Model a
init items defaultValue =
    { items = items
    , visibleItems = items
    , value = defaultValue
    , isOpen = False
    , searchValue = ""
    , canClear = True
    }


{-| @docs Empty model helper - Can be used to initialize an empty Model
-}
empty : Model a
empty =
    { items = []
    , visibleItems = []
    , value = Nothing
    , isOpen = False
    , searchValue = ""
    , canClear = False
    }


{-| @docs Returns the selected value from the dropdown
-}
getValue : Model a -> Maybe a
getValue model =
    case model.value of
        Nothing ->
            Nothing

        Just ( _, x ) ->
            Just x


{-| @docs Helper to generate new state of dropdown with the provided Model from the user
-}
updateItems : Model a -> List ( String, a ) -> Maybe ( String, a ) -> Model a
updateItems model items selected =
    let
        model1 =
            { model | items = items, value = selected }
    in
        update (SearchValue model1.searchValue) model1


{-|
@docs Internal model of the dropdown.
-}
type alias Model a =
    { items : List ( String, a )
    , value : Maybe ( String, a )
    , visibleItems : List ( String, a )
    , isOpen : Bool
    , searchValue : String
    , canClear : Bool
    }


{-|
@docs Dropdown view
-}
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


{-|
@docs Search View takes a the model as input to display the value that is being searched for
-}
searchView : Model a -> Html (Msg a)
searchView model =
    input [ onInput SearchValue, onEnterKey EnteredValue, value model.searchValue ] []


{-|
@docs Values View is the view for all different items in the dropdown
-}
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


{-|
@docs Value View is the view for the individual item in the dropdown
-}
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


{-|
@docs Selected View is the view for the item that is selected
-}
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


{-| -}
onClear : Msg a -> Attribute (Msg a)
onClear msg =
    onWithOptions "click"
        { stopPropagation = True
        , preventDefault = True
        }
        (Json.succeed msg)


{-| -}
canClear : Bool -> String -> Bool
canClear allowed val =
    not (allowed && not (val == ""))


{-| @docs View to render the arrow in the dropdown
-}
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


{-| -}
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


{-| -}
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
