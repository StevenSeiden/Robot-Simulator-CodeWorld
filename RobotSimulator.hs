
-------------------------------------------------------------------------------
-- Global configuration values
-------------------------------------------------------------------------------

theGrid = newGrid(numCells, (-10, 10), (10,-10))

numCells = 10


startX(random) = 1 --truncation(1 + random#1 * theGrid.#cells)
startY(random) = 1 --truncation(1 + random#2 * theGrid.#cells)


theCheckerboard = checkerboard(theGrid)

place = placedInBoard(theGrid)

exercise = 3 -- Change it to another number to test the corresponding exercise

-------------------------------------------------------------------------------
-- Exercise 1: Add a "pause" command for the robot
-------------------------------------------------------------------------------

pause = (0,0)
-- replace undefined with the correct definition

-------------------------------------------------------------------------------
-- Exercise 2: Add a parameter to make the simulation run faster or slower
-------------------------------------------------------------------------------

simulation_speed = 1 -- Change this number for testing exercise 2

-------------------------------------------------------------------------------
-- Exercise 3: Make the robot visit all the cells in the board
-------------------------------------------------------------------------------

robot_plan_ex3 = repeated(visit_all_right(9) ++ visit_all_left(9),5) ++ [up]
  where
  visit_all_left(num) = repeated([left], num) ++ [down]
  visit_all_right(num) = repeated([right], num) ++ [down]

-------------------------------------------------------------------------------
-- Exercise 4: Repeat exercise 4 using a different plan for the robot movement
--             Note: simple transformations of the previous plan do not count
--                   as new plans
-------------------------------------------------------------------------------
robot_plan_ex4 = iterateNums((theGrid.#cells-1))
                 --  ++ right_down(2) ++ visit_all_left(1)
  where
  visit_all_left(num)  = repeated([left], num)
  visit_all_right(num) = repeated([right], num)
  visit_all_up(num)    = repeated([up], num)
  visit_all_down(num)  = repeated([down], num)
  right_down(num) = (visit_all_right(num) ++ visit_all_down(num))
  left_up(num)    = (visit_all_left(num) ++ visit_all_up(num-1)) 
  
  iterateNums :: Number -> [(Number,Number)]
  iterateNums(runAmount) =
     if runAmount <= 0 then [pause]
     else right_down(runAmount)
          ++ left_up(runAmount)
          ++ [right]
          ++ iterateNums(runAmount-2)
          
-------------------------------------------------------------------------------
-- Exercise 5: Make everything work in a board of arbitrary size
--             The board needs to be fit to the size of the output
--             Do not have parts of the board outside the window
--             Test it with 7 cells and with 11 cells
--             Also, change your plan3 and plan4 to depend on numCells
-------------------------------------------------------------------------------

robot_plan_ex5 = cover_all(theGrid.#cells)
  where
  visit_all_left(num)  = repeated([left], num)
  visit_all_right(num) = repeated([right], num)
  visit_all_up(num)    = repeated([up], num)
  visit_all_down(num)  = repeated([down], num)
  right_down(num) = (visit_all_right(num) ++ visit_all_down(num-1))
  left_up(num)    = (visit_all_left(num)  ++ visit_all_up(num-1))
  
  cover_all(num) = 
    if num == 1 then [pause]
    else
      visit_all_right(num-1)
      ++ visit_all_down(num-1)
      ++ left_up(num-1)
      ++ iterateNums((theGrid.#cells-2))

  iterateNums :: Number -> [(Number,Number)]
  iterateNums(runAmount)
    | runAmount == 0 = [pause]
    | runAmount == 1 = [right]
    | runAmount == 2 = [right, right, down, left]
    | otherwise = right_down(runAmount)
                  ++ left_up(runAmount-1)
                  ++ iterateNums(runAmount-2)

  
-------------------------------------------------------------------------------
-- Exercise 6: Make the robot move smoothly instead of jumping
-------------------------------------------------------------------------------

smooth = False -- Use this parameter to control smoothness

-------------------------------------------------------------------------------
-- Exercise 7: Draw a line along the path that the robot moves, so that
--             it is easy to see the trayectory it follows
-------------------------------------------------------------------------------

robotPath = True

-------------------------------------------------------------------------------
-- Exercise 8: Add a plan, like in Exercise 4, so that the robot can start 
--             at any random position and still cover all the cells
--             You may need to modify the state to account for randomness
-------------------------------------------------------------------------------

robot_plan_ex8(random) =
  repeated([up], startX(random)-1)
  ++ repeated([left], startY(random)-1)
  ++ repeated(visit_all_right((numCells'-1))
  ++ [down]
  ++ visit_all_left((numCells'-1))
  ++ [down],4)
  ++ visit_all_right((numCells'-1))
  ++ if remainder(numCells',2) == 0 
     then [down] ++ visit_all_left((numCells'-1))
     else []
  where
  numCells' = theGrid.#cells
  visit_all_left(num) = repeated([left], num)
  visit_all_right(num) = repeated([right], num)


-------------------------------------------------------------------------------
-- Exercise 9: Make the robot move like a chess knight and then plan a path
--             that covers all the cells with such moves
-------------------------------------------------------------------------------
robot_plan_ex9 = 
   [knight(7)
   ,knight(8)
   ,knight(6)
   ,knight(1)
   ,knight(5)
   ,knight(3)
   ,knight(4)
   ,knight(6)
   ,knight(2)
   ,knight(5)
   ,knight(7)
   ,knight(4)
   ,knight(8)
   ,knight(2)
   ,knight(1)
   ,knight(7)
   ,knight(4)
   ,knight(7)
   ,knight(2)
   ,knight(1)
   ,knight(8)
   ,knight(6)
   ,knight(3)
   ,knight(4)
   ]


-------------------------------------------------------------------------------
-- Exercise 10: Add 5 or more obstacles to the grid and replan the paths,
--              so that the robot avoids the obstacles but still covers all
--              the other cells. Each obstacle is assumed to block 1 cell.
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Exercise 11: Make the obstacle locations random. Then simulate a robot that 
--              can sense the obstacles only when it is one cell away from them.
--              Then make the robot traverse all the cells while avoiding 
--              all the obstacles
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Exercise 12: Add more than 1 robot, and make the robots avoid each other
--              and any obstacles while they roam around the grid, assuming
--              their sensors see within a 2-cell radius.
-------------------------------------------------------------------------------





-------------------------------------------------------------------------------
-- State, update and draw
-------------------------------------------------------------------------------

data State = State
  { elapsed :: Number
  , rowPos :: Number
  , colPos :: Number
  , step :: Number
  , crumbs :: [Crumb]
  , obstacle :: Obstacle
  , command :: Command
  }

type Obstacle = Point

type Command = (Number,Number)
type Crumb = (Number,Number,Number)

update :: (State,Number) -> State
update(state,dt)
  | remainder(numCells,2) == 1 && state.#rowPos == numCells && state.#colPos == numCells = state
  | remainder(numCells,2) == 0 && state.#rowPos == numCells && state.#colPos == 1 = state
  | simulation_speed * state.#elapsed < 1 = state{ elapsed = state.#elapsed + dt }
  -- | state.#rowPos == numCells  = state{ rowPos = state.#rowPos - 1}
  | remainder(state.#rowPos,2) == 0 = 
                                      if (rowNew,colNew) == (rPos,cPos+2) then state
                                        {command = up--([(colDir,rowDir),(rowDir,colDir),
                                                  --(rowDir,colDir),(-colDir,-rowDir)]
                                        }
                                      else if (rowNew,colNew) == (rPos+1,cPos+2) then state
                                        {command = right
                                        }
                                      else if (rowNew,colNew) == (rPos+2,cPos+2) then state
                                        {command = right
                                        }
                                      else if (rowNew,colNew) == (rPos+3,cPos+2) then state
                                        {command = down
                                        }
                                      else if state.#colPos == 1 then state{rowPos = state.#rowPos + 1} 
                                      else state{colPos = state.#colPos-1 }
  | remainder(state.#rowPos,2) /= 0 = if (rowNew,colNew) == (rPos,cPos) then state
                                        {command = up
                                        }
                                        else if (rowNew+1,colNew) == (rPos,cPos) then state
                                        {command = left
                                        }
                                        else if (rowNew,colNew) == (rPos,cPos+2) then state
                                        {command = left
                                        }
                                        else if (rowNew,colNew) == (rPos,cPos+3) then state
                                        {command = left
                                        }
                                      else if state.#colPos == numCells then state{rowPos = state.#rowPos + 1} 
                                      else state{colPos = state.#colPos+1}
  |otherwise = state

  
  | otherwise = state
                  { elapsed = 0
                  , rowPos = rowNew
                  , colPos = colNew
                  , step = state.#step + 1
                  , crumbs = (state.#step + 1,rowNew,colNew) : (state.#crumbs)
                  --, command = (rowNew2nd, colNew2nd)
                  , command = (rowNew, colNew)
                  }

  where
    rowNew = state.#rowPos + ci
    colNew = state.#colPos + cj
    rowNew2nd = rowNew + cii
    colNew2nd = colNew + cjj
    (ci,cj) = if(state.#colPos == numCells) then down
      else right
    (cii,cjj) = state.#command
    --obstacle positions are cPos and rPos
    (rPos,cPos) = state.#obstacle
    rowDir = (rowNew-state.#rowPos)
    colDir = (colNew-state.#colPos)

reverseCommand(dirR, dirC) = (-dirR,-dirC)  

draw :: State -> Picture
draw(state) = place(robot,newI,newJ)
            & (if(exercise /= 9) then place(solidRectangle(1,1),yPos,xPos) else blank)
            & draw_crumbs(state.#crumbs,newI,newJ)
            -- & place(solidRectangle(1,1),rowNew2nd,colNew2nd)
            & theCheckerboard

  where
    (yPos,xPos) = state.#obstacle
    (ci,cj) = state.#command
    i = state.#rowPos
    j = state.#colPos
    t = state.#elapsed
    newI = if smooth then i+ci*t else i
    newJ = if smooth then j+cj*t else j
    (rowNew2nd,colNew2nd) = state.#command


draw_crumbs(hs,newI,newJ) =
  if robotPath
    then
    polyline([ ( midpoint(theGrid.#x0, theGrid.#gw, j)
               , midpoint(theGrid.#y0, theGrid.#gh, i) )
             | (n,i,j) <- (0,newI,newJ) : hs ])
    else 
      pictures([ txt(n, midpoint(theGrid.#x0, theGrid.#gw, j)
                    , midpoint(theGrid.#y0, theGrid.#gh, i) )
             | (n,i,j) <- hs ])
  where
    scale = 5 / theGrid.#cells
    txt(n,x,y) = translated(scaled(label,scale,scale),x,y)
      where
      label = text(printed(n))

main = interactionOf(initial,update,handle,draw)

handle :: (State,Event) -> State
handle(state,event) = state

-------------------------------------------------------------------------------
-- Commands and initial plans
-------------------------------------------------------------------------------

down = (1,0)
up = (-1,0)
left = (0,-1)
right = (0,1)

knight(1) = (2,1)
knight(2) = (2,-1)
knight(3) = (-2,1)
knight(4) = (-2,-1)
knight(5) = (1,2)
knight(6) = (1,-2)
knight(7) = (-1,2)
knight(8) = (-1,-2)

robot_plan_ex0 =
  repeated([right],9) ++ [down,down,left,up]
  
robot_plan_ex1 =
  [right,right,right,pause,down,down,pause,pause,down,left,up,left,up,left]
      
initial :: [Number] -> State
initial(random) = State
  { elapsed = 0
  , rowPos = startX(random)
  , colPos = startY(random)
  , step = 1
  , crumbs = [firstCrumb]
  , obstacle = (2,5)
  , command = right
  }
  where
    firstCrumb = (1,startX(random),startY(random))
    cmds(1)     = robot_plan_ex1
    cmds(3)     = robot_plan_ex3
    cmds(4)     = robot_plan_ex4
    cmds(5)     = robot_plan_ex5
    cmds(8)     = robot_plan_ex8(random)
    cmds(9)     = robot_plan_ex9
    cmds(other) = robot_plan_ex0  -- plan 0 is provided as an example

-------------------------------------------------------------------------------
-- Grid
-------------------------------------------------------------------------------
    
data Grid = Grid
  { cells :: Number
  , x0 :: Number -- left
  , y0 :: Number -- top
  , x1 :: Number -- right
  , y1 :: Number -- bottom
  , gw :: Number -- width
  , gh :: Number -- height
  }

newGrid :: (Number,Point,Point) -> Grid
newGrid(n,(leftSide,top),(rightSide,bottom)) = Grid
  { cells = n
  , x0 = leftSide
  , y0 = top
  , x1 = rightSide
  , y1 = bottom
  , gw = (rightSide - leftSide) / n
  , gh = (bottom - top) / n
  }
  
{-| placedInBoard(grid)(pic,i,j) is a picture standing at cell (i,j)
    in a checkerboard(grid), where i is the row and j is the column
    Rows and columns start at index 1. Rows go down and columns go right. |-}
placedInBoard :: Grid -> (Picture,Number,Number) -> Picture
placedInBoard(grid)(pic,i,j) =
  translated(pic',midpoint(grid.#x0,grid.#gw,j),midpoint(grid.#y0,grid.#gh,i))
  where
  pic' = scaled(pic,s,s)
  s = 10 / grid.#cells
  
checkerboard :: Grid -> Picture
checkerboard(grid) =
  pictures
  ([ colored(shifted(row,col),color(row,col))
   | row <- [1..grid.#cells], col <- [1..grid.#cells]
   ])
  where
    color(row,col) = [white,grey(0.8)] # (1 + remainder(row+col,2))
    shifted(row,col) = translated(cell,by(grid.#y0, grid.#gh, col)
                                      ,by(grid.#x0, grid.#gw, row) )
      where
      cell = solidRectangle(grid.#gw,grid.#gh)
      by(start,len,units) = midpoint(start,len,units)
  
-------------------------------------------------------------------------------
-- Other
-------------------------------------------------------------------------------

{-|
    midpoint(x0,w,i) is the location of the midpoint of
    the i-th cell in a 1-dimensional grid anchored at x0, where each cell
    has width w
|-}
midpoint(x0,w,i) = x0 + w/2 + (i-1)*w

-- robot is a picture of a robot (centered at the origin)
robot =
  colored(translated(solidRectangle(0.3,0.1),0,0.1),blue)
  &
  colored(translated(solidRectangle(0.3,0.1),0,-0.1),blue)
  &
  colored(translated(solidRectangle(0.3,0.3),0,0.45)
          & solidRectangle(0.6,0.6),red)
  & translated(solidCircle(0.1),-0.2,-0.45)
  & translated(solidCircle(0.1),0.2,-0.45)

-----------

x .# f = f(x)
