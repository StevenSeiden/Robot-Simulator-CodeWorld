import Extras((<$>))

-------------------------------------------------------------------------------
-- Before proceeding, read all the comments below and pay attention to them
-- In particular, notice a comment that contains the following sentence:
--                  "DO NOT MODIFY ANYTHING BELOW THIS LINE"
-- 
-- Read the rest of that comment carefully and make sure you understand it.
-------------------------------------------------------------------------------





exercise = 8 -- Change it to another number to test the corresponding exercise

-------------------------------------------------------------------------------
-- Exercise 1: Add a "pause" command for the robot
-------------------------------------------------------------------------------

pause = (0,0)
-- replace undefined with the correct definition

-------------------------------------------------------------------------------
-- Exercise 2: Add a parameter to make the simulation run faster or slower
-------------------------------------------------------------------------------

simulation_speed = 3 -- Change this number for testing exercise 2

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
robot_plan_ex4 = iterateNums((numCells-1))

              --  ++ right_down(2) ++ visit_all_left(1)
  where
  visit_all_left(num) = repeated([left], num)
  visit_all_right(num) = repeated([right], num)
  visit_all_up(num) = repeated([up], num)
  visit_all_down(num) = repeated([down], num)
  right_down(num) = (visit_all_right(num) ++ visit_all_down(num))
  left_up(num) = (visit_all_left(num) ++ visit_all_up(num-1)) 
  iterateNums :: Number -> [(Number,Number)]
  iterateNums(runAmount) =
     if runAmount <= 0 then [pause]
     else right_down(runAmount)++ left_up(runAmount) ++ [right] ++ iterateNums(runAmount-2)
-------------------------------------------------------------------------------
-- Exercise 5: Make everything work in a board of arbitrary size
--             The board needs to be fit to the size of the output
--             Do not have parts of the board outside the window
--             Test it with 7 cells and with 11 cells
--             Also, change your plan3 and plan4 to depend on numCells
-------------------------------------------------------------------------------

numCells = 10 -- When you change this number, everything should still work

robot_plan_ex5 = cover_all(numCells)
  where
  visit_all_left(num) = repeated([left], num)
  visit_all_right(num) = repeated([right], num)
  visit_all_up(num) = repeated([up], num)
  visit_all_down(num) = repeated([down], num)
  right_down(num) = (visit_all_right(num) ++ visit_all_down(num-1))
  left_up(num) = (visit_all_left(num) ++ visit_all_up(num-1))
  cover_all(num) = if num == 1 then [pause] else
    visit_all_right(num-1) ++ visit_all_down(num-1)
    ++ left_up(num-1) ++ iterateNums((numCells-2))
     where
     iterateNums :: Number -> [(Number,Number)]
     iterateNums(runAmount) =
       if runAmount <= 2 then
       if runAmount == 1 then [right]
       else if runAmount == 2 then [right, right, down, left]
       else [pause]
     else right_down(runAmount) ++
       left_up(runAmount-1) ++ iterateNums(runAmount-2)

  
-------------------------------------------------------------------------------
-- Exercise 6: Make the robot move smoothly instead of jumping
-------------------------------------------------------------------------------

smooth = False -- Use this parameter to control smoothness

-------------------------------------------------------------------------------
-- Exercise 7: Draw a line along the path that the robot moves, so that
--             it is easy to see the trayectory it follows
-------------------------------------------------------------------------------

robotPath = False

-------------------------------------------------------------------------------
-- Exercise 8: Add a plan, like in Exercise 4, so that the robot can start 
--             at any random position and still cover all the cells
--             You may need to modify the state to account for randomness
-------------------------------------------------------------------------------

startX(random) = 3 --truncation(1 + random#1 * numCells)
startY(random) = 3 --truncation(1 + random#2 * numCells)

robot_plan_ex8(random) = repeated([up], startX(random)-1) ++ repeated([left], startY(random)-1)
  ++repeated(visit_all_right(9) ++ visit_all_left(9),5) ++ [up]
  where
  visit_all_left(num) = repeated([left], num) ++ [down]
  visit_all_right(num) = repeated([right], num) ++ [down]


-------------------------------------------------------------------------------
-- Exercise 9: Make the robot move like a chess knight and then plan a path
--             that covers all the cells with such moves
-------------------------------------------------------------------------------

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
-- Modify update and draw to comply with the requirements above
-------------------------------------------------------------------------------
-- t - time
-- i - row position
-- j - column position
-- cmds - commands 
-- n - step #
-- hs - list of crumbs and position
-- crumb - (n,i,j)
update((t,i,j,[],n,hs),dt) = (t+dt,i,j,[],n,hs)

update((t, i, j, cmds, n, hs), dt) =
  if t*simulation_speed < 1 then (t+dt, i, j, cmds, n, hs)
  else (0, i+ci, j+cj, cmds', n+1, (n+1,i+ci,j+cj):hs)
  where
    (ci,cj) = cmds#1
    cmds' = rest(cmds,1)


draw(t,i,j,cmds,n,hs) =
  scaled(placedInBoard(cells,x0,x1,y0,y1,robot,newI,newJ),10/numCells,10/numCells)
  & scaled(draw_crumbs(cells,x0,x1,y0,y1,hs,newI,newJ),10/numCells,10/numCells)
  & scaled(checkerboard(cells,x0,x1,y0,y1),10/numCells,10/numCells)
  where
    (ci,cj) = cmds#1
    newI = if smooth then i+ci*t else i
    newJ = if smooth then j+cj*t else j
    (cells,x0,x1,y0,y1) = (numCells,-(numCells),numCells,numCells,-(numCells))

draw_crumbs(cells,x0,x1,y0,y1,hs,newI,newJ) = if robotPath then
  polyline[(midpoint(x0,w,j),midpoint(y0,h,i))| (n,i,j) <- (0,newI,newJ):hs]
  else pictures([txt(n,midpoint(x0,w,j),midpoint(y0,h,i))
           | (n,i,j) <- hs ])
  where
    txt(n,x,y) = translated(scaled(text(printed(n)),0.5,0.5),x,y)
    w = (x1-x0)/cells
    h = (y1-y0)/cells

robot_plan_ex0 =
  repeated([right],9) ++ [down,down,left,up]
  
robot_plan_ex1 =
  [right,right,right,pause,down,down,pause,pause,down,left,up,left,up,left]
      

main = interactionOf(initial,update,handle,draw)

handle(state,event) = state

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

initial(random) = (0,startX(random),startY(random),cmds(exercise),1,[firstCrumb])
  where
    firstCrumb = (1,startX(random),startY(random))
    cmds(1)     = robot_plan_ex1
    cmds(3)     = robot_plan_ex3
    cmds(4)     = robot_plan_ex4
    cmds(5)     = robot_plan_ex5
    cmds(8)     = robot_plan_ex8(random)
    cmds(other) = robot_plan_ex0  -- plan 0 is provided as an example

    

{-| placedInBoard(n,x0,x1,y0,y1,pic,i,j) is a picture standing at cell (i,j)
    in a checkerboard(n,x0,x1,y0,y1), where i is the row and j is the column
    Rows and columns start at index 1. Rows go down and columns go right. |-}
placedInBoard(n,x0,x1,y0,y1,pic,i,j) =
  translated(pic,midpoint(x0,w,j),midpoint(y0,h,i))
  where
    w = (x1-x0)/n
    h = (y1-y0)/n

{-| checkerboard(n,x0,x1,y0,y1) is a drawing of a checkerboard with
    n rows and n columns, with top left corner at (x0,y0)
    and bottom right corner at (x1,y1) |-}
checkerboard :: (Number,Number,Number,Number,Number) -> Picture
checkerboard(n,x0,x1,y0,y1) =
  pictures
  ([ colored(translated(solidRectangle(w,h),cx(j),cy(i)),color(i,j))
   | i <- [1..n], j <- [1..n]
   ])
  where
    w = (x1-x0)/n
    h = (y1-y0)/n
    cx(i) = midpoint(x0,w,i)
    cy(j) = midpoint(y0,h,j)
    color(i,j) = [white,grey(0.8)] # (1 + remainder(i+j,2))
  
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
