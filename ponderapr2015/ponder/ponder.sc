object ponder {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  // The Captain Hook team wins if any one Hook can see at least one Peter Pan,
  // but no Wendy, when looking in the direction of any one of his 26 immediate neighbors.
  
  
  // 161 pans out of 343 = 7^3 nodes
  // each node is either a pan (true) or wendy (false)
  // Each node has 13 directions.  27 cubes = 3^3, is 26 adjacencies
  // and two directions per adjacency is 13 directions.
  // The redundancy count of a pan node is the minimum
  // number of other pans amongth the 13 directions.
  // The needed count of a wendy node is the number of directions
  // of that node that have no pans.
  // If the maximum redundancy is 1, then we can't do any better.
  // otherwise, swap the pan with a maximum redundancy with a wendy
  // with the maximum needed wendy node.
  
}