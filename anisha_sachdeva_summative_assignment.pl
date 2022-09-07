/*  Summative Assignment - A Domain Specific Language for Poster Design 
    Prof. Geraint Wiggins
    Jan 22, 2022
    By: Anisha Sachdeva (057183) 
*/

:- use_module([io]).
:- use_module([library(clpfd),library(lists)]).

/* Please refer to last if you want to run the program first. It is given at last how to run the program. */

/*** Definite Clause Grammer Parsing : Summary ***/ 

/* From the poster markup document specified by the user and using the IO library code provided with the project specification, 
we first get a list of Atoms delimited on the following characters: : (colon), \" (quotes), \n (newline), \s (space), \t (tab). 
Next, we write the following DCG parser to parse the list of Atoms. 

We start by writing a recursive predicate named as list_of_constraints/3 which is used to construct a big list of all the atoms parsed 
via DCG. list_of_constraints/3 calls asset/3 which we designed to unify with the value for asset information of each asset type.
We define the two separate definitions of asset/3. 

The first asset/3 predicate is again a recursive call forming a list of property values mentioned for the first asset. 
From the indentation rules (already mentioned) we know that there will be a first asset which would not start with any space or a new line. 
The second asset/3 predicate is written to handle the rest of the assets whose name would particularly be lead by a new line. We know 
from the indentation rules that from second asset onwards the asset name is written in a new line. 
While forming the list of asset information i.e. the related properties of an asset, the first element of the list is the name of the asset.
We do so in order to simplify for us to recognize which properties belong to which asset.

Our DCG is general enough to parse a poster with:
1)'n' number of assets
2) any asset can be placed at any position (though it should follow the specified indentation rules) 
3) there may be 'm' instances of same asset. For example, title asset might be present 5 times in a poster
4) we have not hard coded the names of assets, so in future our DCG can handle any asset's name change or introduction of new asset

In our DCG parsing, the first asset can be anything (not specifically a poster asset). We do so just to generalize our DCG . 
But it is assumed the the fist asset is always going to be a poster asset as later in CLP part, we tend to extract the 
dimensions and filename from the first element of our final dcg output. However, our DCG wont fail if there is any other asset taking the
first position in any poster file. There may be a case if we get another asset as 'PosterDimensions' containing the dimensions of the poster
at the first position. In such case also our DCC can very well extract the dimensions of the poster.  

Further, we define a dcg predicate as asset_info which calls another recursive predicate as list_of_specifications.

Now, as we unify the value of an asset's name with either of the asset/3 predicate, we parse through asset properties extracting them.
list_of_specifications basically forms a list of asset property values and send back those values in a list to asset_info which further 
sends it to asset/3 which further add the property values along with the asset name (as a list) to our final dcg output.

As we do need to mention the explicit name of properties in our dcg to extract them, our DCG is compatible with only the
11 properties mentioned in the project specification. For example, we need, 
specification(Dimensions) --> ['\n', '\t'], [dimensions] , [:], dimensions_to_be_extracted(Dimensions).
Here we have to mention the explicit name [dimensions], hence we cant generalize it further. 

Next, we have implemented some minute details as following:

1) The dimensions of a poster are always going to be a positive number. If by mistake there is a poster specification with say alphabet
or a negative number as a dimension, our dcg will fail immedeiately.
2) The filename mentioned in poster specification should always end with 'html' extension in order to ensure only an HTML poster will be 
printed at the end of the project. If there is any other extension mentioned, our dcg will fail. 
3) The values that position property can take are only 8, namely, 
top-edge, bottom-edge, left-edge, right-edge, top-left, top-right, bottom-left, bottom-right. If there is any other value written while
defining the position, our dcg will fail. 
4) While extracting the width and height we check it should be a positive integer.
5) While extracting the widthPercentage and heightPercentage we check it should be a positive integer and it should lie between 0 and 101,
(both 0 and 101 exclusive).
6) The source which is a relative path to an image source should always end with extensions, JPEG, JPG, GIF, PNG and SVG. We are just 
considering these image extensions, if there is any other extension given, our dcg will fail. This is to ensure that a path to a pdf 
file is not mentioned in source instead of an image.
7) Size and aspect should be positive integers only. 
8) The values that adjacency property can take are only 4, namely, 
above, below, leftof, rightof. If there is any other value specified our dcg will fail. 
*/
 
% list_of_constraints(-AssetList, +Atoms, [])

list_of_constraints([]) --> [].
list_of_constraints([CurrentAssetInfo| NextAssetInfo]) --> asset(CurrentAssetInfo),list_of_constraints(NextAssetInfo).

asset([Poster | PosterInfo]) --> [Poster], [:], asset_info(PosterInfo).
asset([AssetName | AssetInfo]) --> ['\n'], [AssetName], [:], asset_info(AssetInfo).

asset_info(AssetInfo) --> list_of_specifications(AssetInfo).

list_of_specifications([]) --> [].
list_of_specifications([FirstInfo | NextInfo]) --> specification(FirstInfo), list_of_specifications(NextInfo).

specification(Dimensions) --> ['\n', '\t'], [dimensions] , [:], dimensions_to_be_extracted(Dimensions).
specification(Filename) --> ['\n', '\t'], [filename] , [:], filename_to_be_extracted(Filename).
specification(Position) --> ['\n', '\t'], [position] , [:], position_to_be_extracted(Position).
specification(Width) --> ['\n', '\t'], [width] , [:], width_to_be_extracted(Width).
specification(Height) --> ['\n', '\t'], [height] , [:], height_to_be_extracted(Height).
specification(Source) --> ['\n', '\t'], [source] , [:], source_to_be_extracted(Source).
specification(AspectRatio) --> ['\n', '\t'], [aspect] , [:], aspect_to_be_extracted(AspectRatio).
specification(Size) --> ['\n', '\t'], [size] , [:], size_to_be_extracted(Size).
specification(Ref) --> ['\n', '\t'], [ref] , [:], ref_to_be_extracted(Ref).
specification(Adjacency) --> ['\n', '\t'], [adjacency] , [:], adjacency_to_be_extracted(Adjacency).
specification(Content) --> ['\n', '\t'], [content] , [:], content_to_be_extracted(Content).

dimensions_to_be_extracted(rowXcol(RowInt,ColumnInt)) --> [RowXCol], 
                                                    {atomic_list_concat([Row,Column], 'x', RowXCol)},
                                                    {atom_number(Row, RowInt), RowInt>0},
                                                    {atom_number(Column, ColumnInt), ColumnInt>0}.

filename_to_be_extracted(filename(Filename)) --> ['"'], [Filename], ['"'],
                                                    {atomic_list_concat([_, Extension], '.', Filename), Extension = 'html'} .

position_to_be_extracted(position(Position)) --> [Position], {Position = 'top-edge'}.
position_to_be_extracted(position(Position)) --> [Position], {Position = 'bottom-edge'}.
position_to_be_extracted(position(Position)) --> [Position], {Position = 'left-edge'}.
position_to_be_extracted(position(Position)) --> [Position], {Position = 'right-edge'}.
position_to_be_extracted(position(Position)) --> [Position], {Position = 'top-left'}.
position_to_be_extracted(position(Position)) --> [Position], {Position = 'top-right'}.
position_to_be_extracted(position(Position)) --> [Position], {Position = 'bottom-left'}.
position_to_be_extracted(position(Position)) --> [Position], {Position = 'bottom-right'}.

width_to_be_extracted(widthPercent(WidthInt)) --> [WidthPercent], {atom_concat(Width, '%', WidthPercent), 
                                                                    atom_number(Width, WidthInt), WidthInt>0, WidthInt<101}.
width_to_be_extracted(widthAbsolute(WidthInt)) --> [Width], {atom_number(Width, WidthInt), WidthInt>0}.

height_to_be_extracted(heightPercent(HeightInt)) --> [HeightPercent], {atom_concat(Height,'%', HeightPercent),
                                                                        atom_number(Height, HeightInt),HeightInt>0, HeightInt<101}.
height_to_be_extracted(heightAbsolute(HeightInt)) --> [Height], {atom_number(Height, HeightInt), HeightInt>0}.

source_to_be_extracted(source(Source)) --> ['"'], [Source], ['"'],
                                            {atomic_list_concat([_,Extension],'.', Source),Extension ='jpg'}.
source_to_be_extracted(source(Source)) --> ['"'], [Source], ['"'],
                                            {atomic_list_concat([_,Extension],'.', Source),Extension ='jpeg'}.
source_to_be_extracted(source(Source)) --> ['"'], [Source], ['"'],
                                            {atomic_list_concat([_,Extension],'.', Source),Extension ='png'}.
source_to_be_extracted(source(Source)) --> ['"'], [Source], ['"'],
                                            {atomic_list_concat([_,Extension],'.', Source),Extension ='gif'}.
source_to_be_extracted(source(Source)) --> ['"'], [Source], ['"'],
                                            {atomic_list_concat([_,Extension],'.', Source),Extension ='svg'}.                                                                                                                                    

aspect_to_be_extracted(wXhAspect(WidthAspectInt, HeightAspectInt)) --> [WidthXHeight],
                                                                 {atomic_list_concat([WidthAspect, HeightAspect],'x', WidthXHeight)},
                                                                 {atom_number(WidthAspect, WidthAspectInt), WidthAspectInt>0},
                                                                 {atom_number(HeightAspect, HeightAspectInt), HeightAspectInt>0}.

size_to_be_extracted(wXhAbsoluteSize(WidthAbsoluteInt, HeightAbsoluteInt)) --> [WidthXHeight],
                                                                    {atomic_list_concat([WidthAbsolute, HeightAbsolute],'x',WidthXHeight)},
                                                                    {atom_number(WidthAbsolute, WidthAbsoluteInt), WidthAbsoluteInt>0},
                                                                    {atom_number(HeightAbsolute,HeightAbsoluteInt), HeightAbsoluteInt>0}.

ref_to_be_extracted(reference(Ref)) --> ['"'], list_of_words(RefList), ['"'], {atomic_list_concat(RefList, ' ', Ref)}.

adjacency_to_be_extracted(adjacency(Adjacency, Ref)) --> [Adjacency], ['"'], list_of_words(RefList), ['"'], 
                                                                {Adjacency = 'above', atomic_list_concat(RefList, ' ', Ref)}.
adjacency_to_be_extracted(adjacency(Adjacency, Ref)) --> [Adjacency], ['"'], list_of_words(RefList), ['"'], 
                                                                {Adjacency = 'below', atomic_list_concat(RefList, ' ', Ref)}.
adjacency_to_be_extracted(adjacency(Adjacency, Ref)) --> [Adjacency], ['"'], list_of_words(RefList), ['"'], 
                                                                {Adjacency = 'leftof', atomic_list_concat(RefList, ' ', Ref)}.
adjacency_to_be_extracted(adjacency(Adjacency, Ref)) --> [Adjacency], ['"'], list_of_words(RefList), ['"'], 
                                                                {Adjacency = 'rightof', atomic_list_concat(RefList, ' ', Ref)}.                                                                                                                                
content_to_be_extracted(content(Content)) --> ['"'], list_of_words(ContentList), ['"'], {atomic_list_concat(ContentList, ' ', Content)}.

list_of_words([]) --> [].
list_of_words([Word | NextWord]) --> [Word], list_of_words(NextWord), {Word \= '"'}.

/*** End Of DCG ***/

/*** Constraint System ***/

/* 
First, from the complete list_of_constraints (DCG output) we extract the values for TotalRow, TotalColumn and Filename.
Here we assume that the first element of the dcg output list will contain the dimessions and filename.
Hence we assumed in the begining that the first asset in any poster specification is going to be a 'poster' consisting of properties 
such as dimensions and filename. 
We take the first element of the dcg output and extract as required. 
*/

% get_poster_list(+AssetList, -PosterList)
get_poster_list([PosterList | _], PosterList).

% get_dimensions(+Posterlist, -TotalRow, - TotalColumn)
get_dimensions(PosterList,TotalRow,TotalColumn) :- member(rowXcol(TotalRow,TotalColumn), PosterList).

% get_filename(+PosterList, -Filename)
get_filename(PosterList,Filename) :- member(filename(Filename), PosterList).

/* 
Once we extract the dimensions and filename, we no longer need the information about the poster asset. We need to generate boxes from 
the remaining assets. 
Poster asset will not be a box. Hence, we remove the poster information and now work with the new list which does not contain the poster 
asset.
*/

% remove_poster_list(+AssetList, -AssetListWOPoster)
remove_poster_list([_| AssetListWOPoster], AssetListWOPoster).

/* 
We define the required predicates box_id, box_type, box_row, box_col, box_content to generate the general structure of a box for each asset.
General structure of box looks like, box(id = Id, type = Type, content = Content, row = (R0, R1), col = (C0,C1)).
*/

box_id(box(id = Id, type = _, content = _, row = (_, _), col = (_,_)), Id).
box_type(box(id = _, type = Type, content = _, row = (_, _), col = (_,_)), Type).
box_row(box(id = _, type = _, content = _, row = (R0, R1), col = (_,_)), R0, R1).
box_col(box(id = _, type = _, content = _, row = (_,_), col = (C0,C1)), C0, C1).
box_content(box(id = _, type = _, content = Content, row = (_, _), col = (_,_)), Content).

type_header('header').
type_text('text').
type_image('image').

/*
We write a recursive predicate to generate boxes. 
*/

% generate_boxes(+AssetListWOPoster, -Boxes)
generate_boxes([],[]).
generate_boxes([AssetList | AssetLists], [Box | Boxes]) :- box(AssetList, Box), generate_boxes(AssetLists, Boxes).

% box(+OneAsset, -BoxWithFilledAssetAndContent)
box(AssetList, Box) :- fill_asset_type(AssetList, Box), fill_asset_content(AssetList, Box).

/*
We fill the asset type as header, text or image and fill the content from the dcg output.
*/

% fill_asset_type(+AssetListWOPoster, -BoxFilledWithType)
fill_asset_type(AssetList, box(id = _, type = Type, content = _, row = (_, _), col = (_,_))) :- member(title, AssetList), type_header(Type).
fill_asset_type(AssetList, box(id = _, type = Type, content = _, row = (_, _), col = (_,_))) :- member(section, AssetList), type_header(Type).
fill_asset_type(AssetList, box(id = _, type = Type, content = _, row = (_, _), col = (_,_))) :- member(caption, AssetList), type_text(Type).
fill_asset_type(AssetList, box(id = _, type = Type, content = _, row = (_, _), col = (_,_))) :- member(text, AssetList), type_text(Type).
fill_asset_type(AssetList, box(id = _, type = Type, content = _, row = (_, _), col = (_,_))) :- member(image, AssetList), type_image(Type).
fill_asset_type(AssetList, box(id = _, type = Type, content = _, row = (_, _), col = (_,_))) :- member(figure, AssetList), type_image(Type).

% fill_asset_content(+AssetListWOPoster, -BoxFilledWithContent)
fill_asset_content(AssetList, box(id = _, type = _, content = Content, row = (_, _), col = (_,_))) :- member(content(Content), AssetList).
fill_asset_content(AssetList, box(id = _, type = _, content = Source, row = (_,_), col = (_,_))) :- member(source(Source), AssetList).

/* After boxes are generated, we added the Ids */

% fill_asset_id(?Boxes)
fill_asset_id(Boxes) :- asset_id(Boxes, 1).
asset_id([], _).
asset_id([Box | Boxes], Id) :- box_id(Box, Id), NewId is Id +1, asset_id(Boxes, NewId).

/* We generate pairs of Ref-Box. We will use these pairs later on while constraining adjacency.

For example: if we have a poster specification like, 
poster:
  dimensions: 25x35
  filename: "volcano.html"
image:
  source: "lava.jpeg"
  ref: "lava"
  position: left-edge
  width: 50%
title:
  position: top-edge
  width: 100%
  content: "Volcanoes"
caption:
  content: "Fountain of lava erupting from a volcanic cone in Hawaii, 1983."
  adjacency: below "lava"
section:
  position: right-edge
  content: "Location"
  ref: "location"

Image asset will be box 1, title asset will be box 2, caption asset will be box 3 and section will be box 4. 
Now in box 3, adjacency, when we extract 'below "lava"' from the dcg, 
we need to look in the list where was "lava" was mentioned as the reference. 
Reference may or may not be specified in the consecutive boxes. Like in our case reference is mentioned in box 1 where as adjacency is mentioned
in box 3. To handle such cases, if we will have a list of only Reference-Box pairs, we can easily locate the box_id for the 
corresponding reference. 

In this case, ref-box pair would look like, 

FlattenedReferenceBoxList = [lava-box(id=1, type=image, content='lava.jpeg', row=(_20994, _20996), col=(_21006, _21008)), 
location-box(id=4, type=header, content='Location', row=(_21186, _21188), col=(_21198, _21200))] 

We can easily find from here that 'lava' reference corresponds to box_id 1 and hence the caption i.e. box 3 needs to be below box 1.
We can apply apply this in constraints parts. 

We generate these ref-box pairs to handle cases wherein the reference and adjacency might not be mentioned in consecutive boxes.

*/

% generate_pair(+Boxes, +AssetListWOPoster, -Ref-BoxPair)
generate_pair([], [], []).
generate_pair([Box|Boxes], [AssetList|AssetLists], [Reference - Box|References]) :-
                                                member(reference(Reference), AssetList), generate_pair(Boxes, AssetLists, References). 

generate_pair([_|Boxes], [AssetList|AssetLists], [References]) :-
                                                \+member(reference(_), AssetList), generate_pair(Boxes, AssetLists, References).

flatten_pair_list(ReferenceBoxList, FlattenedReferenceBoxList) :- flatten(ReferenceBoxList, FlattenedReferenceBoxList).                                               

/* 
constrain_boxes is a recursive method to call one box from the entire list of Boxes and create a list of constraints for it. 
*/

% constrain_boxes(+Boxes, -ConstrainedAssetLists, +TotalRow, +TotalColumn, +FlattenedReferenceBoxList)
constrain_boxes([], [], _ , _, _).
constrain_boxes([Box | Boxes], [ConstrainedAssetList | ConstrainedAssetLists], TotalRow, TotalColumn, FlattenedReferenceBoxList) :- 
                                        constrain_box(Box, ConstrainedAssetList, TotalRow, TotalColumn, FlattenedReferenceBoxList),
                                        constrain_boxes(Boxes, ConstrainedAssetLists, TotalRow, TotalColumn, FlattenedReferenceBoxList).

/* 
constrain_box is a recursive call in 'each box' to check various assets and put constraints on them. We use the call built-in here. 
As in the dcg we had mentioned the name of each asset and property while extracting their values, we could now use them as predicates
using the call built-in. 
*/

% constrain_box(+Box, -ConstrainedAssets, +TotalRow, +TotalColumn, +FlattenedReferenceBoxList)
constrain_box(_, [], _, _, _).
constrain_box(Box, [ConstrainedAsset|ConstrainedAssets],  TotalRow, TotalColumn, FlattenedReferenceBoxList) :- 
                                        call(ConstrainedAsset, Box, TotalRow, TotalColumn, FlattenedReferenceBoxList), 
                                        constrain_box(Box, ConstrainedAssets, TotalRow, TotalColumn, FlattenedReferenceBoxList).
/* 
We put some global constraints like the value of each Row should lie between 1 to TotalRow + 1 (we add one here as TotalRow is exclusive).
*/

% global_row_column_constraints()
global_row_column_constraints([], _, _).
global_row_column_constraints([Box | Boxes], TotalRow, TotalColumn) :- box_row(Box, R0, R1), box_col(Box, C0, C1), 
                                                                        RowLabel is TotalRow + 1,
                                                                        ColumnLabel is TotalColumn+1,
                                                                        R0 in 1..RowLabel, R1 in 1..RowLabel,
                                                                        C0 in 1..ColumnLabel, C1 in 1..ColumnLabel,
                                                                        R1 #> R0, C1 #> C0,
                                                                        global_row_column_constraints(Boxes, TotalRow, TotalColumn).

/* To overcome the issue of overlapping of boxes, we designed two recursive preidcates working on same logic. 
One for handling the rows of the box and other handling the columns. 
In overlapping_rows predicate, we take two consecutive boxes and make the end row of the first box as the starting row of the 
second box. As we are doing this for all the consecutive boxes, at the end there wont be any over lap. 
We added another case of overlapping_rows predicate wherein the starting point of the second box may be 1. This is also a possibility 
when there may be two consecutive boxes wherein the first box would be at the position Top-Left and another box at Top-Edge. In that
scenario both can share row as 1 because they both can still have different starting point. Say the first box starts at C0 =1 till C1 = 3
at Top Left corner, the other box may start at C2 = 4 till C3 = 6 at the Top Edge. 

However, we know this overlapping solution has it's own limitations and is not full-proof. We have tested for various conditions where 
still there could be overlapping. Like if two boxes have top-left and top-edge position with 100% width, they can overlap. 
Also, this implementation gives correct adjacency but if we say Box 2 should be at right of Box 1, 
then Box 2 will be in the right side but it will be below Box 1. This is because we are explicitely telling in the constraint 
that consecutive boxes need to below each other (to prevant overlapping).

This is not the complete solution for overlapping, but this the best we could do given the time constraint. 

*/
overlapping_rows([]).
overlapping_rows([_]).
overlapping_rows([Box1, Box2 | Boxes]) :- box_row(Box1, _, R1), box_row(Box2, R2, _), R2 #= R1, 
                                                overlapping_rows([Box2 | Boxes]).
overlapping_rows([_, Box2 | Boxes]) :- box_row(Box2, R2, _), R2 #= 1, 
                                          overlapping_rows([Box2 | Boxes]).



overlapping_col([]).
overlapping_col([_]).
overlapping_col([Box1, Box2 | Boxes]) :- box_col(Box1, _, C1), box_col(Box2, C2, _), C2 #= C1,
                                                overlapping_col([Box2 | Boxes]).
overlapping_col([_, Box2 | Boxes]) :- box_col(Box2, C2, _), C2 #= 1,
                                         overlapping_col([Box2 | Boxes]).

/* In the below section we finally put the constraints.
As there is nothing to be extracted and put constraint on the asset types, we mentioned their arguments as '_'. It doesnt matter to us
whatever value they may have. 
A normal predicate would look like asset_property(PropertyValue, Box, TotalRow, TotalColumn, FlattenedReferenceBoxList) and then we can 
call box_row(Box, R0, R1), box_col(Box, C0, C1) and put constraint on that particular property. 
*/

title(_, _, _, _).
text(_, _, _, _).
figure(_, _, _, _).
image(_, _, _, _).
caption(_, _, _, _).
section(_, _, _, _).
content(_, _, _, _, _).


/* 
When the call (built-in) calls the predicate position, it would check if the property value matches 'top-edge', the constraint
that R0 of the box i.e. the first row of the box should be 1 (top-row) will be applied. 
Similarly we apply constrainsts for the other possibilities. 

Note: We tried to label while constraining only but it proved to be a very inefficient method. One variable may get labelled during 
the application of first constraint but later on may be after 5 constraints, there might be a need to label it again. 
So it adds up to the inefficiency and latency in results. Hence, we labelled at the end after the completing the constraints. 
*/

position('top-edge', Box, _, _, _) :- box_row(Box, R0, _), R0 #= 1.

position('bottom-edge', Box, TotalRow, _, _) :- box_row(Box, _, R1), R1 #= (TotalRow+1).

position('left-edge', Box, _, _, _) :- box_col(Box,C0,_), C0 #= 1. 

position('right-edge', Box, _, TotalColumn, _) :- box_col(Box,_,C1), C1 #= (TotalColumn+1).

position('top-left', Box, _, _, _) :- box_row(Box, R0, _), box_col(Box, C0, _), 
                                        R0 #= 1, C0 #=1.

position('top-right', Box, _, TotalColumn, _) :- box_row(Box, R0, _), box_col(Box, _, C1), 
                                        R0 #= 1, C1 #= (TotalColumn+1).

position('bottom-left', Box, TotalRow, _, _) :- box_row(Box, _, R1), box_col(Box, C0, _),
                                         R1 #= (TotalRow+1), C0 #=1.

position('bottom-right', Box, TotalRow, TotalColumn, _) :- box_row(Box, _, R1), box_col(Box, _, C1), 
                                        R1 #= (TotalRow+1), C1 #= (TotalColumn+1).

widthPercent(WidthInt, Box, _, TotalColumn, _) :- box_col(Box, C0, C1), 
                                        WidthFloat is ((WidthInt/100) * TotalColumn), 
                                        ceiling(WidthFloat, Width), Width #= (C1 - C0).

widthAbsolute(WidthInt, Box, _, _, _) :- box_col(Box, C0, C1), WidthInt #= (C1 - C0).

heightPercent(HeightInt, Box, TotalRow, _, _) :- box_row(Box, R0, R1), 
                                        HeightFloat is ((HeightInt/100) * TotalRow), 
                                        ceiling(HeightFloat, Height), Height #= (R1 - R0).

heightAbsolute(HeightInt, Box, _, _, _) :- box_row(Box, R0, R1), HeightInt #= (R1 - R0).

source(_, _, _, _, _).

wXhAspect(WidthAspectInt, _, Box, _, _, _) :- box_row(Box, R0, R1), box_col(Box, C0, C1),
                                                                             (C1 - C0) #= (WidthAspectInt * (R1 - R0)).

wXhAbsoluteSize(WidthAbsoluteInt, HeightAbsoluteInt, Box, _, _, _) :- box_row(Box, R0, R1), box_col(Box, C0, C1),
                                                WidthAbsoluteInt #= (C1 - C0), 
                                                HeightAbsoluteInt #= (R1 - R0).

reference(_, _, _, _, _).

adjacency(above, Ref, Box, _, _, FlattenedReferenceBoxList) :- member(Ref-Box2,FlattenedReferenceBoxList), 
                                                                       box_row(Box2, R0, _), box_row(Box, _, R3),
                                                                       R0 #= R3.

adjacency(below, Ref, Box, _, _, FlattenedReferenceBoxList) :- member(Ref-Box2,FlattenedReferenceBoxList), 
                                                                       box_row(Box2, _, R1), box_row(Box, R2, _),
                                                                       R1 #= R2.

adjacency(leftof, Ref, Box, _, _ ,FlattenedReferenceBoxList) :- member(Ref-Box2,FlattenedReferenceBoxList), 
                                                                       box_col(Box2, C0, _), box_col(Box, _, C3), 
                                                                       C0 #= C3.
                                                                    
adjacency(rightof, Ref, Box, _, _ ,FlattenedReferenceBoxList) :- member(Ref-Box2,FlattenedReferenceBoxList), 
                                                                       box_col(Box2, _, C1), box_col(Box, C2, _), 
                                                                       C1 #= C2.

/* At last we make a recursive predicate to label the Row and Column Variables in each box. While labeling we use a little 
heuristic which says the boxes for type header and title should be minimized and the boxes for type image should be maximized 
to provide a better visual. Plain text may look boring, but images wont. 
*/ 

% labelling_box(?Boxes)
labelling_box([]).
labelling_box([Box | Boxes]) :- label_box(Box), labelling_box(Boxes).

label_box(Box) :- box_type(Box, header), box_row(Box, R0, R1), box_col(Box, C0, C1), 
                        labeling([min(C1-C0), min(R1-R0)],[C0,C1,R0, R1]).

label_box(Box) :- box_type(Box, text), box_row(Box, R0, R1), box_col(Box, C0, C1), 
                        labeling([min(C1-C0), min(R1-R0)],[C0,C1,R0, R1]).

label_box(Box) :- box_type(Box, image), box_row(Box, R0, R1), box_col(Box, C0, C1), 
                        labeling([max(C1-C0), max(R1-R0)],[C0,C1,R0, R1]).

/* 
layout_boxes was a predicate to be specified in our program in order to utilize the output capabilities of the IO file. 
It was quite late when I could understand what 'Layout' meant, hence I declared the layout_boxes which takes the labelled boxes
as input and gives those same boxes as output. This enabled me to use the output predicate defined in the IO file. 
*/

% layout_boxes(?Boxes, ?Boxes)
layout_boxes(Boxes,Boxes).

/* 
*** How to run the program *** 

If working on Mac, change the working directory of SWIPL to the directory where this file is currently present. 
The following command can be used. 

 % working_directory(_, 'directory where the file is currentlyp present').

Please copy paste the predicates given below to your SWIPL and run after loading this .pl file. Please note here I have specified poster
to be spec.poster.

input('spec.poster', Atoms), 
list_of_constraints(AssetList, Atoms, []), 
get_poster_list(AssetList,PosterList), 
get_dimensions(PosterList,TotalRow, TotalColumn),                  
get_filename(PosterList,Filename),
remove_poster_list(AssetList,AssetListWOPoster),    
generate_boxes(AssetListWOPoster,Boxes),                   
fill_asset_id(Boxes),                  
generate_pair(Boxes,AssetListWOPoster,ReferenceBoxList), 
flatten_pair_list(ReferenceBoxList, FlattenedReferenceBoxList), 
global_row_column_constraints(Boxes, TotalRow, TotalColumn),
overlapping_rows(Boxes), 
overlapping_col(Boxes),
constrain_boxes(Boxes, AssetListWOPoster, TotalRow, TotalColumn, FlattenedReferenceBoxList), 
labelling_box(Boxes),
output(Boxes,Filename,TotalColumn,TotalRow).
*/