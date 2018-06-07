module Main exposing (..)

import Color as Colors
import Element exposing (Element, Grid, el, empty, grid, layout, text)
import Element.Attributes exposing (center, padding, px, spacing, verticalCenter)
import Html exposing (Html)
import List
import List.Extra as ListE
import Maybe.Extra as MaybeE
import Style exposing (..)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font


-- MODEL


type Player
    = X
    | O


type alias Cell =
    { player : Player, x : Int, y : Int }


type alias Pieces =
    List Cell


type alias Pos a =
    { a | x : Int, y : Int }


type alias Board a =
    { a | pieces : Pieces, selected : Maybe Cell }


type alias Model =
    { pieces : Pieces, turn : Player, selected : Maybe Cell, winner : Maybe Player }


initialModel : Model
initialModel =
    { pieces = [ { x = 0, y = 0, player = O }, { x = 2, y = 2, player = O } ], turn = X, selected = Just { x = 1, y = 1, player = X }, winner = Nothing }


setPos : Pos a -> Cell -> Cell
setPos pos cell =
    { cell | x = pos.x, y = pos.y }


posEq : Pos a -> Pos b -> Bool
posEq pos pos2 =
    pos.x == pos2.x && pos.y == pos2.y


getCell : Pos a -> { b | pieces : Pieces } -> Maybe Cell
getCell pos model =
    List.head (List.filter (posEq pos) model.pieces)


removeCell : Pos b -> { a | pieces : Pieces } -> { a | pieces : Pieces }
removeCell pos model =
    { model | pieces = List.filter (\c -> not (posEq c pos)) model.pieces }


getSelected : Board b -> Pos a -> Maybe Cell
getSelected model pos =
    MaybeE.filter (posEq pos) model.selected



-- UPDATE


type Msg
    = AddPiece Cell
    | MovePiece { x : Int, y : Int }
    | SelectPiece Cell


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddPiece c ->
            { model | pieces = c :: model.pieces }

        MovePiece p ->
            case model.selected of
                Just cell ->
                    { model | selected = Nothing, pieces = setPos p cell :: model.pieces }

                Nothing ->
                    model

        SelectPiece p ->
            { model | selected = getCell p model, pieces = (removeCell p model).pieces }



-- VIEW


type Styles
    = NoStyle
    | CellStyle
    | SelectedCellStyle


cellBaseSheet : List (Property Styles variation)
cellBaseSheet =
    [ Border.all 1
    , Border.solid
    , Border.rounded 5
    , Font.size 60
    ]


sheet : StyleSheet Styles variation
sheet =
    Style.styleSheet
        [ style NoStyle []
        , style CellStyle
            cellBaseSheet
        , style SelectedCellStyle
            (Color.background Colors.lightGray
                :: cellBaseSheet
            )
        ]


view : Model -> Html Msg
view model =
    Element.layout sheet <|
        viewBoard model


viewBoard : Board b -> Element Styles variation Msg
viewBoard model =
    el
        NoStyle
        [ center, verticalCenter ]
        (grid
            NoStyle
            [ spacing 10 ]
            { rows = List.repeat 3 (px 100)
            , columns = List.repeat 3 (px 100)
            , cells = viewCells model
            }
        )


viewCells : Board b -> List (Element.OnGrid (Element Styles variation Msg))
viewCells board =
    List.map
        (\p ->
            case p of
                [ x, y ] ->
                    let
                        pos =
                            { x = x, y = y }
                    in
                    Maybe.withDefault (viewEmptyCell pos)
                        (MaybeE.orElse
                            (Maybe.map (viewCell SelectedCellStyle) (getSelected board pos))
                            (Maybe.map (viewCell CellStyle) (getCell pos board))
                        )

                _ ->
                    Debug.crash "not a coordinate"
        )
        (ListE.cartesianProduct
            [ [ 0, 1, 2 ], [ 0, 1, 2 ] ]
        )


viewCell : Styles -> Cell -> Element.OnGrid (Element Styles variation Msg)
viewCell styles cell =
    Element.cell { start = ( cell.x, cell.y ), width = 1, height = 1, content = el styles [ center, verticalCenter, padding 20 ] (viewPlayer cell.player) }


viewEmptyCell : Pos a -> Element.OnGrid (Element Styles variation Msg)
viewEmptyCell pos =
    Element.cell { start = ( pos.x, pos.y ), width = 1, height = 1, content = el CellStyle [] empty }


viewPlayer : Player -> Element Styles variation Msg
viewPlayer player =
    case player of
        X ->
            text "X"

        O ->
            text "O"


main =
    Html.beginnerProgram
        { model = initialModel
        , view = view
        , update = update
        }
