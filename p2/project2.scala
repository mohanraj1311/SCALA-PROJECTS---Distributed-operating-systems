import scala.actors.Actor
import scala.actors.Actor._
import scala.util._



object project2 {
def main(args:Array[String]):Unit={
//No. of nodes should be at least '4'
    if(args(1) == "2D" || args(1) == "imp2D"){
      if(args(0).toInt < 4){
        println("No. of nodes should be minimum '4' for 2D or imp2D. Input again!")
        return
      }
    }
    //Checking for any other algorithm than gossip or push-sum
    if(args(2) != "gossip" && args(2) != "push-sum"){
      println("Supported algorithms are:\n1.gossip\n2.push-sum\nThese names are case sensitive. Try again!")
      return
    }
    //Initializing the main actor */
    //val masterObj = new masterClass(5,"full","gossip")
    val masterObj = new masterClass(args(0).toInt,args(1),args(2))
    masterObj.start
}
}

case class message(sValue:Double,wValue:Double,msgString:String){}
class gridStructure(hasLeft: Boolean,hasTop: Boolean,hasRight: Boolean,hasBottom: Boolean,indexOfRandomNode: Int){
  var gridHasLeft: Boolean = hasLeft
  var gridHasTop: Boolean = hasTop
  var gridHasRight: Boolean = hasRight
  var gridHasBottom: Boolean = hasBottom
  var gridIndexRandom: Int = indexOfRandomNode
}

class calculate{
  //To check whether it's a perfect square or not
  def isPerfectSq(number: Int): Boolean = {
    val sqaureRoot = scala.math.sqrt(number)
    if((sqaureRoot%1)>0)
      return false
    else
      return true
  }
  //To generate Random number
  def generateRandom(seed: Int): Int = {
    return new Random().nextInt(seed)
  }
}

class masterClass(numOfTerminals:Int,topology:String,algo:String) extends Actor {
  
  var numOfresponses:Int=0;
  
  def act(){
    var noOfTerminals:Int=numOfTerminals
    var calculate= new calculate();
    
    //line Topology
    if(topology=="line"){
      var terminalArray:Array[Terminal]=new Array[Terminal](noOfTerminals)
      //initialization of terminals
      
      for(i:Int<-0 until noOfTerminals){
        //special condition:1st terminal and last terminal
        if(i==0||i==(noOfTerminals-1)){
          val terminal=new Terminal("Terminal"+i,algo,i.toDouble,1.0,new Array[Terminal](1),this)
          terminalArray(i)=terminal;
          }
        else{
          val terminal=new Terminal("Terminal"+i,algo,i.toDouble,1.0,new Array[Terminal](2),this)
          terminalArray(i)=terminal;
        }
        
      }
      
      println("------- TERMINALS INITIALIZED FOR LINE ------------")
      println("---------------------------------------------------")
      
      //Assign neighbors
      for(j:Int<-0 until noOfTerminals){
        if(j==0){
          //Left ending terminal
          terminalArray(j).fixNeighbors(1,Array[Terminal](terminalArray(j+1)))
        }
        else if(j==noOfTerminals-1){
          //Right ending terminal
          terminalArray(j).fixNeighbors(1,Array[Terminal](terminalArray(j-1)))
        }
        else{
        	//all other terminals
        	terminalArray(j).fixNeighbors(2,Array[Terminal](terminalArray(j-1),terminalArray(j+1)))
        }
        
        //For the ease of understanding:
        //Current terminal
        println("current terminal name: "+terminalArray(j).terName)
        //List of neighbors
        if(terminalArray(j).TerNeighbors.size > 1){
          println("first neighbor: "+terminalArray(j).TerNeighbors(0).terName)
          println("second neighbor: "+terminalArray(j).TerNeighbors(1).terName)
        }else
          println("only neighbor: "+terminalArray(j).TerNeighbors(0).terName)
        
      }
      
      println("-------------- LINE TOPOLOGY IS BUILT ------------")
      println("--------------------------------------------------")
      //Choosing a random terminal to start with
      var randomStartIndex: Int = calculate.generateRandom(noOfTerminals)
      println("Random start terminal: "+randomStartIndex)
      terminalArray(randomStartIndex).theReceiverActor ! new message(0.0,0.0,"rumor")
 
      
    }
    
    //Full Network Topology
    else if(topology == "full"){
      //The terminal array
      var terminalArray: Array[Terminal] = new Array[Terminal](noOfTerminals)
      //Initialization of Terminals
      for(i: Int <- 0 until noOfTerminals){
        val terminal = new Terminal("Terminal"+i,algo,i.toDouble,1.0,new Array[Terminal](noOfTerminals-1),this)
        terminalArray(i) = terminal
      }
      println("------------- TERMINAL INITIALIZATION ENDS --------")
      println("---------------------------------------------------")
      // Fixing of neighbors
      for(j: Int <- 0 until noOfTerminals){
        var neighborArray: Array[Terminal] = new Array[Terminal](noOfTerminals-1)
        var m: Int = 0
        for(k: Int <- 0 until (noOfTerminals-1)){
          if(j==k)
            m += 1
          neighborArray(k) = terminalArray(m)
          m += 1
        }	
        terminalArray(j).fixNeighbors(noOfTerminals - 1,neighborArray)
        //For the ease of understanding:
        //Current terminal
        println("current terminal name:!!! "+terminalArray(j).terName)
        //List of neighbors
        for(l: Int <- 0 until (noOfTerminals-1))
          println("neighbor: "+terminalArray(j).TerNeighbors(l).terName)
      }
      println("-------FULL NETWORK TOPOLOGY IS BUILT ------")
      println("--------------------------------------------")
      //Choosing a random terminal to start with
      var randomStartIndex: Int = calculate.generateRandom(noOfTerminals)
      println("Random start terminal: "+randomStartIndex)
      terminalArray(randomStartIndex).theReceiverActor ! new message(0.0,0.0,"rumor")
    }
    
    //2D Grid Topology
    else if(topology == "2D"){
      //Taking the nearest perfect square smaller than the currently entered number
      while(!calculate.isPerfectSq(noOfTerminals)){
        noOfTerminals -= 1
      }
      //The terminal array
      var terminalArray: Array[Terminal] = new Array[Terminal](noOfTerminals)
      //Instantiating the Grid Structure Array
      var gridStructureArray: Array[gridStructure] = new Array[gridStructure](noOfTerminals)
      //Instantiating Grid Structures for every terminal of the Array
      for(p: Int <- 0 until noOfTerminals){
        var myGS: gridStructure = new gridStructure(false,false,false,false,0)
        gridStructureArray(p) = myGS
      }
      //Initialization of terminals
      for(i: Int <- 0 until noOfTerminals){
        if((i % (scala.math.sqrt(noOfTerminals)) == 0) && (i <= (scala.math.sqrt(noOfTerminals)-1))){
          //Should not have both left and top neighbors
          val terminal = new Terminal("terminal"+i,algo,i.toDouble,1.0,new Array[Terminal](2),this)
          terminalArray(i) = terminal
          gridStructureArray(i).gridHasLeft = false
          gridStructureArray(i).gridHasTop = false
          gridStructureArray(i).gridHasRight = true
          gridStructureArray(i).gridHasBottom = true
        }else if((i <= (scala.math.sqrt(noOfTerminals)-1)) && ((i+1) % (scala.math.sqrt(noOfTerminals)) == 0)){
          //Should not have both top and right neighbors
          val terminal = new Terminal("Terminal"+i,algo,i.toDouble,1.0,new Array[Terminal](2),this)
          terminalArray(i) = terminal
          gridStructureArray(i).gridHasLeft = true
          gridStructureArray(i).gridHasTop = false
          gridStructureArray(i).gridHasRight = false
          gridStructureArray(i).gridHasBottom = true
        }else if(((i+1) % (scala.math.sqrt(noOfTerminals)) == 0) && (i >= (scala.math.sqrt(noOfTerminals)-1)*(scala.math.sqrt(noOfTerminals)))){
          //Should not have both right and bottom neighbors
          val terminal = new Terminal("Terminal"+i,algo,i.toDouble,1.0,new Array[Terminal](2),this)
          terminalArray(i) = terminal
          gridStructureArray(i).gridHasLeft = true
          gridStructureArray(i).gridHasTop = true
          gridStructureArray(i).gridHasRight = false
          gridStructureArray(i).gridHasBottom = false
        }else if((i >= (scala.math.sqrt(noOfTerminals)-1)*(scala.math.sqrt(noOfTerminals))) && (i % (scala.math.sqrt(noOfTerminals)) == 0)){
          //Should not have both bottom and left neighbors
          val terminal = new Terminal("Terminal"+i,algo,i.toDouble,1.0,new Array[Terminal](2),this)
          terminalArray(i) = terminal
          gridStructureArray(i).gridHasLeft = false
          gridStructureArray(i).gridHasTop = true
          gridStructureArray(i).gridHasRight = true
          gridStructureArray(i).gridHasBottom = false
        }else if(i % (scala.math.sqrt(noOfTerminals)) == 0){
          //Should not have left neighbor
          val terminal = new Terminal("terminal"+i,algo,i.toDouble,1.0,new Array[Terminal](3),this)
          terminalArray(i) = terminal
          gridStructureArray(i).gridHasLeft = false
          gridStructureArray(i).gridHasTop = true
          gridStructureArray(i).gridHasRight = true
          gridStructureArray(i).gridHasBottom = true
        }else if((i+1) % (scala.math.sqrt(noOfTerminals)) == 0){
          //Should not have right neighbor
          val terminal = new Terminal("terminal"+i,algo,i.toDouble,1.0,new Array[Terminal](3),this)
          terminalArray(i) = terminal
          gridStructureArray(i).gridHasLeft = true
          gridStructureArray(i).gridHasTop = true
          gridStructureArray(i).gridHasRight = false
          gridStructureArray(i).gridHasBottom = true
        }else if(i <= (scala.math.sqrt(noOfTerminals)-1)){
          //Should not have top neighbor
          val terminal = new Terminal("terminal"+i,algo,i.toDouble,1.0,new Array[Terminal](3),this)
          terminalArray(i) = terminal
          gridStructureArray(i).gridHasLeft = true
          gridStructureArray(i).gridHasTop = false
          gridStructureArray(i).gridHasRight = true
          gridStructureArray(i).gridHasBottom = true
        }else if(i >= (scala.math.sqrt(noOfTerminals)-1)*(scala.math.sqrt(noOfTerminals))){
          //Should not have bottom neighbor
          val terminal = new Terminal("terminal"+i,algo,i.toDouble,1.0,new Array[Terminal](3),this)
          terminalArray(i) = terminal
          gridStructureArray(i).gridHasLeft = true
          gridStructureArray(i).gridHasTop = true
          gridStructureArray(i).gridHasRight = true
          gridStructureArray(i).gridHasBottom = false
        }else{
          //Should have all four neighbors
          val terminal = new Terminal("terminal"+i,algo,i.toDouble,1.0,new Array[Terminal](4),this)
          terminalArray(i) = terminal
          gridStructureArray(i).gridHasLeft = true
          gridStructureArray(i).gridHasTop = true
          gridStructureArray(i).gridHasRight = true
          gridStructureArray(i).gridHasBottom = true
        }
      }
      println("------------- terminal INITIALIZATION ENDS ------------")
      println("---------------------------------------------------")
      //Initialization of neighbors
      for(j: Int <- 0 until noOfTerminals){
        //List of Neighbors
        var neighborList = List[Terminal]()
        //Adding to the list one by one from the terminalArray based on whether or not it has got certain neighbors
        if(gridStructureArray(j).gridHasLeft)
          neighborList ::= terminalArray(j-1)
        if(gridStructureArray(j).gridHasTop)
          neighborList ::= terminalArray(j-(scala.math.sqrt(noOfTerminals)).toInt)
        if(gridStructureArray(j).gridHasRight)
          neighborList ::= terminalArray(j+1)
        if(gridStructureArray(j).gridHasBottom)
          neighborList ::= terminalArray(j+(scala.math.sqrt(noOfTerminals)).toInt)
        //Array of neighbors
        var neighborArray: Array[Terminal] = new Array[Terminal](neighborList.size)
        //Adding from the list to the array
        for(l:Int <- 0 until neighborList.size){
          neighborArray(l) = neighborList(l)
        }
        //Assigning neighbors to the terminal
        terminalArray(j).fixNeighbors(neighborList.size,neighborArray)
        //For the ease of understanding:
        //Current terminal
        println("Current terminal: "+terminalArray(j).terName)
        //List of neighbors
        for(p: Int <- 0 until neighborList.size){
          println(p+"-th neighbor: "+neighborList(p).terName)
        }
      }
      //Choosing a random terminal to start with
      var randomStartIndex: Int = calculate.generateRandom(noOfTerminals)
      println("Random start terminal: "+randomStartIndex)
      terminalArray(randomStartIndex).theReceiverActor ! new message(0.0,0.0,"rumor")
    }
    
    //Imperfect 2D Grid Topology
    else if(topology == "imp2D"){
      //Taking the nearest perfect square smaller than the currently entered number
      while(!calculate.isPerfectSq(noOfTerminals)){
        noOfTerminals -= 1
      }
      println("noOfTerminals: "+noOfTerminals)
      //The terminal array
      var terminalArray: Array[Terminal] = new Array[Terminal](noOfTerminals)
      //Instantiating the Grid Structure Array
      var gridStructureArray: Array[gridStructure] = new Array[gridStructure](noOfTerminals)
      //Instantiating Grid Structures for every terminal of the Array
      for(p: Int <- 0 until noOfTerminals){
        var myGS: gridStructure = new gridStructure(false,false,false,false,0)
        gridStructureArray(p) = myGS
      }
      //Initialization of terminals
      for(i: Int <- 0 until noOfTerminals){
        if((i % (scala.math.sqrt(noOfTerminals)) == 0) && (i <= (scala.math.sqrt(noOfTerminals)-1))){
          //Should not have both left and top neighbors
          val terminal = new Terminal("terminal"+i,algo,i.toDouble,1.0,new Array[Terminal](3),this)
          terminalArray(i) = terminal
          gridStructureArray(i).gridHasLeft = false
          gridStructureArray(i).gridHasTop = false
          gridStructureArray(i).gridHasRight = true
          gridStructureArray(i).gridHasBottom = true
          //One random neighbor
          var generatedRandom: Int = calculate.generateRandom(noOfTerminals)
          while(generatedRandom == i ||
              generatedRandom == i+1 ||
              generatedRandom == (i+scala.math.sqrt(noOfTerminals))){
            generatedRandom = calculate.generateRandom(noOfTerminals)
          }
          gridStructureArray(i).gridIndexRandom = generatedRandom
          println(i+"-th terminal | "+generatedRandom)
        }else if((i <= (scala.math.sqrt(noOfTerminals)-1)) && ((i+1) % (scala.math.sqrt(noOfTerminals)) == 0)){
          //Should not have both top and right neighbors
          val terminal = new Terminal("Terminal"+i,algo,i.toDouble,1.0,new Array[Terminal](3),this)
          terminalArray(i) = terminal
          gridStructureArray(i).gridHasLeft = true
          gridStructureArray(i).gridHasTop = false
          gridStructureArray(i).gridHasRight = false
          gridStructureArray(i).gridHasBottom = true
          //One random neighbor
          var generatedRandom: Int = calculate.generateRandom(noOfTerminals)
          while(generatedRandom == i ||
              generatedRandom == i-1 ||
              generatedRandom == (i+scala.math.sqrt(noOfTerminals))){
            generatedRandom = calculate.generateRandom(noOfTerminals)
          }
          gridStructureArray(i).gridIndexRandom = generatedRandom
          println(i+"-th terminal | "+generatedRandom)
        }else if(((i+1) % (scala.math.sqrt(noOfTerminals)) == 0) && (i >= (scala.math.sqrt(noOfTerminals)-1)*(scala.math.sqrt(noOfTerminals)))){
          //Should not have both right and bottom neighbors
          val terminal = new Terminal("Terminal"+i,algo,i.toDouble,1.0,new Array[Terminal](3),this)
          terminalArray(i) = terminal
          gridStructureArray(i).gridHasLeft = true
          gridStructureArray(i).gridHasTop = true
          gridStructureArray(i).gridHasRight = false
          gridStructureArray(i).gridHasBottom = false
          //One random neighbor
          var generatedRandom: Int = calculate.generateRandom(noOfTerminals)
          while(generatedRandom == i ||
              generatedRandom == i-1 ||
              generatedRandom == (i-scala.math.sqrt(noOfTerminals))){
            generatedRandom = calculate.generateRandom(noOfTerminals)
          }
          gridStructureArray(i).gridIndexRandom = generatedRandom
          println(i+"-th terminal | "+generatedRandom)
        }else if((i >= (scala.math.sqrt(noOfTerminals)-1)*(scala.math.sqrt(noOfTerminals))) && (i % (scala.math.sqrt(noOfTerminals)) == 0)){
          //Should not have both bottom and left neighbors
          val terminal = new Terminal("terminal"+i,algo,i.toDouble,1.0,new Array[Terminal](3),this)
          terminalArray(i) = terminal
          gridStructureArray(i).gridHasLeft = false
          gridStructureArray(i).gridHasTop = true
          gridStructureArray(i).gridHasRight = true
          gridStructureArray(i).gridHasBottom = false
          //One random neighbor
          var generatedRandom: Int = calculate.generateRandom(noOfTerminals)
          while(generatedRandom == i ||
              generatedRandom == i+1 ||
              generatedRandom == (i-scala.math.sqrt(noOfTerminals))){
            generatedRandom = calculate.generateRandom(noOfTerminals)
          }
          gridStructureArray(i).gridIndexRandom = generatedRandom
          println(i+"-th terminal | "+generatedRandom)
        }else if(i % (scala.math.sqrt(noOfTerminals)) == 0){
          //Should not have left neighbor
          val terminal = new Terminal("Terminal"+i,algo,i.toDouble,1.0,new Array[Terminal](4),this)
          terminalArray(i) = terminal
          gridStructureArray(i).gridHasLeft = false
          gridStructureArray(i).gridHasTop = true
          gridStructureArray(i).gridHasRight = true
          gridStructureArray(i).gridHasBottom = true
          //One random neighbor
          var generatedRandom: Int = calculate.generateRandom(noOfTerminals)
          while(generatedRandom == i ||
              generatedRandom == i+1 ||
              generatedRandom == (i+scala.math.sqrt(noOfTerminals)) ||
              generatedRandom == (i-scala.math.sqrt(noOfTerminals))){
            generatedRandom = calculate.generateRandom(noOfTerminals)
          }
          gridStructureArray(i).gridIndexRandom = generatedRandom
          println(i+"-th terminal | "+generatedRandom)
        }else if((i+1) % (scala.math.sqrt(noOfTerminals)) == 0){
          //Should not have right neighbor
          val terminal = new Terminal("terminal"+i,algo,i.toDouble,1.0,new Array[Terminal](4),this)
          terminalArray(i) = terminal
          gridStructureArray(i).gridHasLeft = true
          gridStructureArray(i).gridHasTop = true
          gridStructureArray(i).gridHasRight = false
          gridStructureArray(i).gridHasBottom = true
          //One random neighbor
          var generatedRandom: Int = calculate.generateRandom(noOfTerminals)
          while(generatedRandom == i ||
              generatedRandom == i-1 ||
              generatedRandom == (i+scala.math.sqrt(noOfTerminals)) ||
              generatedRandom == (i-scala.math.sqrt(noOfTerminals))){
            generatedRandom = calculate.generateRandom(noOfTerminals)
          }
          gridStructureArray(i).gridIndexRandom = generatedRandom
          println(i+"-th terminal | "+generatedRandom)
        }else if(i <= (scala.math.sqrt(noOfTerminals)-1)){
          //Should not have top neighbor
          val terminal = new Terminal("Terminal"+i,algo,i.toDouble,1.0,new Array[Terminal](4),this)
          terminalArray(i) = terminal
          gridStructureArray(i).gridHasLeft = true
          gridStructureArray(i).gridHasTop = false
          gridStructureArray(i).gridHasRight = true
          gridStructureArray(i).gridHasBottom = true
          //One random neighbor
          var generatedRandom: Int = calculate.generateRandom(noOfTerminals)
          while(generatedRandom == i ||
              generatedRandom == i+1 ||
              generatedRandom == i-1 ||
              generatedRandom == (i+scala.math.sqrt(noOfTerminals))){
            generatedRandom = calculate.generateRandom(noOfTerminals)
          }
          gridStructureArray(i).gridIndexRandom = generatedRandom
          println(i+"-th terminal | "+generatedRandom)
        }else if(i >= (scala.math.sqrt(noOfTerminals)-1)*(scala.math.sqrt(noOfTerminals))){
          //Should not have bottom neighbor
          val terminal = new Terminal("Terminal"+i,algo,i.toDouble,1.0,new Array[Terminal](4),this)
          terminalArray(i) = terminal
          gridStructureArray(i).gridHasLeft = true
          gridStructureArray(i).gridHasTop = true
          gridStructureArray(i).gridHasRight = true
          gridStructureArray(i).gridHasBottom = false
          //One random neighbor
          var generatedRandom: Int = calculate.generateRandom(noOfTerminals)
          while(generatedRandom == i ||
              generatedRandom == i+1 ||
              generatedRandom == i-1 ||
              generatedRandom == (i-scala.math.sqrt(noOfTerminals))){
            generatedRandom = calculate.generateRandom(noOfTerminals)
          }
          gridStructureArray(i).gridIndexRandom = generatedRandom
          println(i+"-th terminal | "+generatedRandom)
          }else{
          //Should have all four neighbors
          val terminal = new Terminal("Node"+i,algo,i.toDouble,1.0,new Array[Terminal](5),this)
          terminalArray(i) = terminal
          gridStructureArray(i).gridHasLeft = true
          gridStructureArray(i).gridHasTop = true
          gridStructureArray(i).gridHasRight = true
          gridStructureArray(i).gridHasBottom = true
          //One random neighbor
          var generatedRandom: Int = calculate.generateRandom(noOfTerminals)
          while(generatedRandom == i ||
              generatedRandom == i+1 ||
              generatedRandom == i-1 ||
              generatedRandom == (i+scala.math.sqrt(noOfTerminals)) ||
              generatedRandom == (i-scala.math.sqrt(noOfTerminals))){
            generatedRandom = calculate.generateRandom(noOfTerminals)
          }
          gridStructureArray(i).gridIndexRandom = generatedRandom
          println(i+"-th Terminal | "+generatedRandom)
        }
    }
      println("------------- TERMINAL INITIALIZATION ENDS ------------")
      println("-----------------------------------------------------")
      //Initialization of neighbors
      for(j: Int <- 0 until noOfTerminals){
        //List of Neighbors
        var neighborList = List[Terminal]()
        //Adding to the list one by one from the terminalArray based on whether or not it has got the neighbors
        //Also adding the random neighbor
        if(gridStructureArray(j).gridHasLeft)
          neighborList ::= terminalArray(j-1)
        if(gridStructureArray(j).gridHasTop)
          neighborList ::= terminalArray(j-(scala.math.sqrt(noOfTerminals)).toInt)
        if(gridStructureArray(j).gridHasRight)
          neighborList ::= terminalArray(j+1)
        if(gridStructureArray(j).gridHasBottom)
          neighborList ::= terminalArray(j+(scala.math.sqrt(noOfTerminals)).toInt)
        neighborList ::= terminalArray(gridStructureArray(j).gridIndexRandom)
        //Array of neighbors
        var neighborArray: Array[Terminal] = new Array[Terminal](neighborList.size)
        //Adding from the list to the array
        for(l:Int <- 0 until neighborList.size){
          neighborArray(l) = neighborList(l)
        }
        //Assigning neighbors to the terminal
        terminalArray(j).fixNeighbors(neighborList.size,neighborArray)
        //For the ease of understanding:
        //Current terminal
        println("Current terminal: "+terminalArray(j).terName)
        //List of neighbors
        for(p: Int <- 0 until neighborList.size){
          println(p+"-th neighbor: "+neighborList(p).terName)
        }
      }
      //Choosing a random terminal to start with
      var randomStartIndex: Int = calculate.generateRandom(noOfTerminals)
      println("Random start terminal: "+randomStartIndex)
      terminalArray(randomStartIndex).theReceiverActor ! new message(0.0,0.0,"rumor")
    }
      
    //No topology matches
    else{
      println("This topology is not supported.\nSuppored topologies are:\n1.line\n2.full\n3.2D\n4.imp2D\nThese names are case sensitive. Try again!")
      exit
    }
    
        //Time counting starts
    val timeAfterTopologyBuilt = System.currentTimeMillis;
    println("The time:"+timeAfterTopologyBuilt)
    //hear responses from the node actors
    loop{
      react{
        case msg: message =>
          numOfresponses += 1
          //Master actor exits only if it gets messages/responses back from all the terminal actors
          if(numOfresponses == noOfTerminals){
            println("---------------------")
            println("MASTER ACTOR EXITS")
            println("---------------------")
            //Printing the total time
            println("Total time taken: "+(System.currentTimeMillis-timeAfterTopologyBuilt)+" ms")
            exit
          }
      }
    }
    
  }
  

}


class Terminal(name: String, algorithm: String, sValue: Double, wValue: Double, neighbors: Array[Terminal], mainActor: Actor){
  
  
  //Property members of the terminal actor class
  var terName: String = name
  var terAlgorithm: String = algorithm
  var TerNeighbors: Array[Terminal] = neighbors
  var messageCount: Int = 0	//Required for Gossip
  var theSenderActor: Actor = this.sendmessage
  var theReceiverActor: Actor = this.receivemessage
  var theMainActor: Actor = mainActor
  
  //Property members especially required for Gossip
  var isAlive: Boolean = true
  var terSValue: Double = sValue
  var terWValue: Double = wValue
  var rCurrent: Double = this.terSValue/this.terWValue
  var rPrevious: Double = -100.0
  var rSPrevious: Double = -200.0
  
  
  def fixNeighbors(noOfNeighbors:Int,listOfNeighbors:Array[Terminal]){
    
    for(i:Int <- 0 until noOfNeighbors){
      this.TerNeighbors(i) = listOfNeighbors(i)
    }
  }
  
    //The Sender Actor
  def sendmessage: Actor = {
    println("inside send message of "+this.terName)
    val theSender = actor{
      loop{
react{
case msg:message =>
if(this.algorithm.equals("gossip")){
//Sender actor runs till it converges
while(this.messageCount < 10){
var randomNeighborIndex: Int = (new calculate()).generateRandom(this.TerNeighbors.size)
this.TerNeighbors(randomNeighborIndex).theReceiverActor ! msg
}
}else if(this.algorithm.equals("push-sum") && !msg.msgString.equals("stop")){
println("sVal | wVal before calculation in Sender: "+this.terSValue+" | "+this.terWValue)
this.terSValue /= 2
this.terWValue /= 2
println("sVal | wVal after calculation in Sender: "+this.terSValue+" | "+this.terWValue)
//Checking if there is any alive neighbor
var isAliveNeighbor: Boolean = false
for(i: Int <- 0 until this.TerNeighbors.size){
if(TerNeighbors(i).isAlive)
isAliveNeighbor = true
}
//Sending to an alive neighbor
if(isAliveNeighbor){
var randomNeighborIndex: Int = (new calculate()).generateRandom(this.TerNeighbors.size)
while(this.TerNeighbors(randomNeighborIndex).isAlive == false){
randomNeighborIndex = (new calculate()).generateRandom(this.TerNeighbors.size)
}
println("randomNeighborIndex: "+randomNeighborIndex+" for "+this.terName)
this.TerNeighbors(randomNeighborIndex).theReceiverActor ! (new message(this.terSValue,this.terWValue,msg.msgString))
}
//Informing the main actor about force exit if no alive neighbor
else{
this.isAlive = false
this.theMainActor ! new message(0.0,0.0,"done")	
println(this.terName+"'s sender and receiver actor terminated forcefully.")
exit
}	
}
if(msg.msgString.equals("stop")){
println(this.terName+" sender exits ")
println("============================")
exit
}
}
      }
    }
    theSender
  }
  
   def receivemessage: Actor = {
    println("inside receive message of "+this.terName)
    val theReceiver = actor{
      loop{
react{
case msg:message =>
//println("msg.msgString "+msg.msgString)	
if(msg.msgString.equals("rumor")){
if(this.algorithm.equals("gossip")){
println("this.messageCount: "+this.messageCount+"for: "+this.terName)
//Converges when the message count reaches 10
if(this.messageCount < 10){
this.messageCount += 1
//Sending to the sender actor while working
this.theSenderActor ! msg
println("sent to the sender")
}else{
//Asking the sender actor to stop and informing the main actor that it has converged
this.theSenderActor ! new message(0.0,0.0,"stop")
this.theMainActor ! new message(0.0,0.0,"done")
println(this.terName+" receiver exits")
println("-----------------------------")
exit
}
}else if(this.algorithm.equals("push-sum")){
//calculates for Push-Sum
println("sval | wVal before in Receiver: "+this.terSValue+" | "+this.terWValue)
this.terSValue += msg.sValue
this.terWValue += msg.wValue	
this.rSPrevious = this.rPrevious
this.rPrevious = this.rCurrent
this.rCurrent = this.terSValue/this.terWValue
println("sVal | wVal after in Receiver: "+this.terSValue+" | "+this.terWValue)
println("this.rCurrent: "+this.rCurrent+" this.rPrevious: "+this.rPrevious+" this.rSPrevious: "+this.rSPrevious+" for: "+this.terName)	
//Converges when the s/w value did not change more than 0.0000000001 for 3 consecutive rounds
if(scala.math.abs(this.rCurrent-this.rPrevious) > 0.0000000001
|| scala.math.abs(this.rCurrent-this.rSPrevious) > 0.0000000001
|| scala.math.abs(this.rSPrevious-this.rPrevious) > 0.0000000001){	
//Passing to the sender actor
this.theSenderActor ! (new message(this.terSValue,this.terWValue,msg.msgString))
println("sent to the sender")
}else{
//Making itself dead
this.isAlive = false
//Passing to sender actor for forwarding before it dies
this.theSenderActor ! (new message(this.terSValue,this.terWValue,msg.msgString))
//Asking the sender actor to stop
this.theSenderActor ! new message(0.0,0.0,"stop")
//Informing the main actor that it has converged
this.theMainActor ! new message(0.0,0.0,"done")
println(this.terName+" receiver exits ")
println("------------------------------")
exit	
}
}	
}
}
      }
    }
    theReceiver
  }
     
     
     
     
   
}

