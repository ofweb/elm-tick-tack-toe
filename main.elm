module Main exposing (..)

import Element exposing (column, el, empty, layout, row, text)
import Html exposing (Html)
import List
import Style exposing (..)


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


type alias BoardPieces a =
    { a | pieces : Pieces }


type alias Model =
    { pieces : Pieces, turn : Player, selected : Maybe Cell, winner : Player }


setPos : Pos a -> Cell -> Cell
setPos pos cell =
    { cell | x = pos.x, y = pos.y }


posEq : Pos a -> Pos b -> Bool
posEq pos pos2 =
    pos.x == pos2.x && pos.y == pos2.y


getCell : Pos a -> BoardPieces a -> Maybe Cell
getCell pos model =
    List.head (List.filter (\c -> posEq c pos) model.pieces)


removeCell : Pos b -> BoardPieces a -> BoardPieces a
removeCell pos model =
    { model | pieces = List.filter (\c -> not (posEq c pos)) model.pieces }


type ViewCell
    = Normal (Maybe Cell)
    | Selected Cell


viewCellFor : Board a -> Pos b -> ViewCell
viewCellFor model pos =
    case model.selected of
        Just cell ->
            if posEq cell pos then
                Selected cell
            else
                Normal (getCell pos model)

        Nothing ->
            Normal (getCell pos model)


mapViewCells : (ViewCell -> a) -> Board b -> Int -> List a
mapViewCells func model x =
    List.map (\y -> func (viewCellFor model { x = x, y = y })) (List.range 0 2)



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
                    { model | selected = Nothing, pieces = setPos p cell :: model }

                Nothing ->
                    model

        SelectPiece p ->
            { model | selected = getCell p model, pieces = removeCell p model }



-- VIEW


type StyleClass
    = NoStyle


sheet =
    Style.styleSheet
        [ style NoStyle []
        ]


view : Model -> Html Msg
view model =
    Element.layout sheet <|
        row NoStyle
            []
            [ column NoStyle
                []
                []
            ]


viewBoard model =
    List.map (\x -> mapViewCells viewCell model x) (List.range 0 2)


viewCell vc =
    case vc of
        Normal Nothing ->
            el NoStyle [] empty

        Normal (Just cell) ->
            el NoStyle [] (text (viewPlayer cell.player))

        Selected cell ->
            el NoStyle [] (text (viewPlayer cell.player))


viewPlayer : Player -> String
viewPlayer player =
    case player of
        X ->
            "X"

        O ->
            "O"
