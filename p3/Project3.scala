import scala.actors.Actor
import scala.actors.Actor._
import scala.math
import scala.actors.remote.Node
import scala.actors.TIMEOUT
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer

object Project3 {
  
  
  
  
  def main(args:Array[String]):Unit={
    var numRequests:Integer=0;
    var numNodes:Double=0;
     if(args.length<1){                           //default values
      numNodes=1024.0;
      numRequests= 10;
     }
     else
     {
     numNodes=args(0).toDouble;     //No of nodes
     numRequests=args(1).toInt;    //no of requests
       
       
     }
   
      println("The No. Of Nodes    :"+numNodes)
    println("The No. Of Requests :"+numRequests)
   
    
    
    var Power:Double=0;                       //convert N into nearest power of two
    Power=math.log(numNodes)/math.log(2);
    Power=Power.ceil
    numNodes=math.pow(2, Power)
  println("Rounding of Number of Nodes to "+numNodes.toInt);
  
  var networkobj=new network(numNodes.toInt,numRequests)
  networkobj.start()
  networkobj!"BuildNetwork"
  }
}

class network(numOfNodes1:Int,numRequests:Int) extends Actor{
  
  var numNodes:Int=numOfNodes1-1;   
  var numOfPeers:Int=numNodes;
  var onePeerLess=numNodes-1;
  //numPeers=128
  val sizebinary=(math.log(numOfPeers+1)/math.log(2)).toInt;
  //println(sizebinary);
  
   var sequence = new ArrayBuffer[Int]()
    
   var fullBuff = new ArrayBuffer[Int]() // will store the random nos
    
   var z:Int = 0
    
   var activePeers:Int = 0
    
   for(i <- 0 to onePeerLess){
      sequence += i
      }
   
   var randomgenerator=new scala.util.Random;
   var rand=randomgenerator.nextInt(onePeerLess);
   var rangeOfnumbers=0 to (onePeerLess);
   
   //Begin: Creation of fullBuff
   while(onePeerLess >= 0){
                if (onePeerLess != 0){
                  //println("reached here")
                        rand = randomgenerator.nextInt(onePeerLess)
                } else {
                        rand = 0
                }
                fullBuff += sequence(rand)
                sequence.remove(rand)
                onePeerLess -= 1
    }
   //End : Creation of fullBuff
   
   
   var Array_of_nodes = new ArrayBuffer[PastryNode](numNodes);
   
   var k:Int=0
   
   val initiator = new Initiator(numOfPeers,fullBuff,Array_of_nodes,numRequests)
   
   initiator.start()
   
   while(k<numNodes){                        //Initialization of conn
         var newNode:PastryNode = null
         newNode = new PastryNode(initiator,sizebinary)
         newNode.start()
         Array_of_nodes += newNode
         k += 1
      }
    //  initiator ! conn
      k = 0
      while(k<numNodes){
           Array_of_nodes(k)!(Array_of_nodes,k) //sending node Id
           k += 1
      }
      k =0
   
    		  while(k<numNodes){
        Array_of_nodes(fullBuff(k)) ! (k,fullBuff,"Sending peer-id and full-rand-array")
        k +=1
      }

      k=0

   
   
   
   
  
  def act(){
        
         loop{
         react{
         case "BuildNetwork" =>
           //initialize 8 actors normally// each actor will return here after initialize so that next one can initialize
                if (k<8) {
                    //println("the node "+ k)
                    Array_of_nodes(fullBuff(k)) ! (k,fullBuff,"Join1",activePeers,activePeers) // for join and calculate both
                    k += 1
                    activePeers += 1
                		} 
                //now do the join// each actor will return here after initialize so that next one can initialize
                else if (k<numNodes) { 
                    rand = randomgenerator.nextInt(k-1)
                    //println("Node joining id : "+ k)
                    //send the new currPeers to all nodes so far in peer network and to update routing and leaf tables.
                    z = 0
                    while(z<=activePeers){
                            Array_of_nodes(fullBuff(z)) ! (z,fullBuff,"Join2",activePeers,k) 
                            z +=1
                    }
                    k += 1
                    activePeers += 1
                }
                if(k == numNodes){
                        exit()
                }
                 }
                 }
    
  }
  
}

class Initiator(numOfPeers:Int,fullBuff:ArrayBuffer[Int],conn:ArrayBuffer[PastryNode],numRequests:Int) extends Actor{
  var PeersReady:Int = 0
  var i:Int = 0
  var sum = Array[Int](numOfPeers+1)
  var randomGenerator = new scala.util.Random
    var random = randomGenerator.nextInt(numOfPeers);
  var finalCount:Int = numRequests*(numOfPeers)
  var recieved_Count:Int = 0
  var j:Int = 0
  sum = Array.fill(numOfPeers+1)(0)
  var count:Double = 0

    def act(){
        loop {
                react{
                 //Begin : Acknowledge case
                        case "acknowledged" =>
                                PeersReady += 1
                                if(PeersReady == numOfPeers) {
                                  println("Network is Built..Sending messages now...")
                                        //ready to send message
                                        println("*************************************************************************");
                                        //Tell Node
                                        j = 0
                                        while(j<numRequests){
                                                i = 0
                                                while(i<numOfPeers){
                                                        //println(full_buff(i) + " sending messages to " + tempRand)
                                                        //conn(full_buff(i)) !(fullBuff(tempRand),1,"MessagePassing","MessagePassing",fullBuff(i))
                                                        conn(fullBuff(i)) !(fullBuff(random),1,"MessagePassing","abcd","abcd",i)
                                                        random = randomGenerator.nextInt(numOfPeers-1)
                                                        i += 1
                                                }
                                                j +=1
                                        }
                                }
                         
                        case terminate:(Int ,Int) =>
                          
                                sum(terminate._1) = sum(terminate._1) + terminate._2
                                recieved_Count += 1

                                if(recieved_Count == finalCount){
                                  j = 0
                                  while(j<numOfPeers+1){
                                    count = count + sum(j)
                                    j = j+1
                                  }

                                 println("Program is converging...")
                                  println("Avg No. of hops for passing "+ numRequests+ " messages for each node = "+ count/((numOfPeers+1)*numRequests))
                                  println("Program is terminating..")
                                  j= 0
                                  while(j<numOfPeers){
                                    conn(fullBuff(j)) ! "exit"
                                    j += 1
                                  }
                                  exit()

                                }
                }
                }
        }

}


class PastryNode(initiator:Initiator,sizebinary:Int) extends Actor {
  
  
 
  var self_id:Int = 0;
  var self_PeerID:Int = 0;
  var isPeer:Int = 0;
  type Row = ArrayBuffer[Int]
  type conn_type = ArrayBuffer[PastryNode]
  val RouteTable = new Array[Row](sizebinary)
  
  var ForwardLeaf = new ArrayBuffer[Int]
 
  var totNumPeers:Int = 127
  var self_Binary:String =""
  var maxLeaf = -1
  var minLeaf = -1
  
  var currPeers:Int = 0
  
  var array_of_nodes = new ArrayBuffer[PastryNode]
  var connection_list= new ArrayBuffer[Int]
 

  var i=0
  while (i<sizebinary){
    RouteTable(i) = new ArrayBuffer[Int]
    i += 1
  }
  
  def act(){
                 loop {
                        react{
                          case "exit" =>
                            exit()
                         case TIMEOUT =>
                        

                         case x :(ArrayBuffer[PastryNode],Int) =>
                           self_id = x._2
                           array_of_nodes = x._1
                         case x: (Int,ArrayBuffer[Int],String) =>
                           
                           self_PeerID = x._1
                           connection_list = x._2
                           isPeer = 1
                            self_Binary = getBinaryOf(x._1)
                          /* println("Received ID:" + x._1 + "=" + myBinary + " " +"Reverse=" + Integer.parseInt(myBinary,2) +" Flipped " + flipBit(myBinary,3))*/
                         case x: (Int,ArrayBuffer[Int],String,Int,Int) =>// receiving Peer ID

                          if(x._3 == "Join1"){ //do routine joins
                             currPeers = x._4
                             var curBit = 0
                             while(curBit<sizebinary){
                                 RouteTable(/*curBit*/0) +=Integer.parseInt(flipBit(self_Binary,(curBit + 1)),2)
                                 curBit += 1
                             }
                             /* Routing Table End */

                             /* Leaf Build */
                              totNumPeers = 7 
                                  if (self_PeerID + 1 > totNumPeers - 1) {
                                          ForwardLeaf += 0;
                                          ForwardLeaf += 1;
                                  } else if(self_PeerID + 2 > totNumPeers -1){
                                          ForwardLeaf += (self_PeerID + 1);
                                          ForwardLeaf += 0;
                                  } else {
                                          ForwardLeaf += (self_PeerID + 1);
                                          ForwardLeaf += (self_PeerID + 2);
                                          maxLeaf = self_PeerID + 2;
                                  }

                                  if (self_PeerID - 1 < 0) {
                                          ForwardLeaf += totNumPeers - 2;
                                          ForwardLeaf += totNumPeers - 1;
                                  } else if(self_PeerID - 2 < 0){
                                          ForwardLeaf += (self_PeerID - 1);
                                          ForwardLeaf += totNumPeers -1;
                                  } else {
                                          ForwardLeaf += (self_PeerID - 1);
                                          ForwardLeaf += (self_PeerID - 2);
                                          minLeaf = self_PeerID - 2;
                                  }

                                  /* ForwardLeaf += (self_PeerID + 1);
									RearLeaf += (self_PeerID + 2);*/

                                  /* Leaf Build End*/


                                  sender ! "BuildNetwork"
                                  initiator ! "acknowledged"
                          } else { currPeers = x._4
                                /* Routing Table */
                                   if( x._1 == x._5)
                                   { var curBit = 0
                                           while(curBit<sizebinary){
                                                   RouteTable(0) += Integer.parseInt(flipBit(self_Binary,(curBit + 1)),2)
                                           curBit += 1
                                           }
                                   }
                             /* Routing Table End */

                             /* Leaf Build */
                              totNumPeers = currPeers 
                              ForwardLeaf.clear
                              if (self_PeerID + 1 > totNumPeers - 1) {
                                          ForwardLeaf += 0;
                                          ForwardLeaf += 1;
                                  } else if(self_PeerID + 2 > totNumPeers -1){
                                          ForwardLeaf += (self_PeerID + 1);
                                          ForwardLeaf += 0;
                                  } else {
                                          ForwardLeaf += (self_PeerID + 1);
                                          ForwardLeaf += (self_PeerID + 2);
                                          maxLeaf = self_PeerID + 2;
                                  }

                                  if (self_PeerID - 1 < 0) {
                                          ForwardLeaf += totNumPeers - 2;
                                          ForwardLeaf += totNumPeers - 1;
                                  } else if(self_PeerID - 2 < 0){
                                          ForwardLeaf += (self_PeerID - 1);
                                          ForwardLeaf += totNumPeers -1;
                                  } else {
                                          ForwardLeaf += (self_PeerID - 1);
                                          ForwardLeaf += (self_PeerID - 2);
                                          minLeaf = self_PeerID - 2;
                                  }

                                  if( x._1 == x._5){
                                    sender ! "BuildNetwork"
                                    initiator ! "acknowledged"
                                  }
                          }

                         case x : (Int,Int,String,String,String,Int) =>   //Routing Logic
                          // println("recevied message passing")
                                if(x._3 == "MessagePassing"){
                                  /* Check Leaf Nodes*/
                                  if (x._1 == self_PeerID) {
                                          //println("Reached destination from "+ x._6 + " in "+x._2 +" hops")
                                	  	//	print("Id:   "+myID+"  ")
                                	  	//	println("No. of Hops to reach destination from "+ x._6 + " = "+x._2 )
                                          initiator ! (x._6,x._2)
                                  } else {
                                        var nextPeer = checkLeafTable(x._1)
                                        if (nextPeer != -999){ // not in leaf

                                                        array_of_nodes(connection_list(nextPeer)) ! (x._1,x._2 + 1,"MessagePassing","abcd","abcd",x._6)

                                        } else {
                                          /* check routing table */

                                          var len = RouteTable(0).size
                                          var i=0
                                          var flag =0
                                          var curr = (getBinaryOf(self_PeerID))

                                          i = getMatchingPrefix(curr,getBinaryOf(x._1))
                                                if (i >= len) {
                                                  
                                                } else {
                                                   array_of_nodes(connection_list(RouteTable(0)(i))) ! (x._1,x._2 + 1,"MessagePassing","abcd","abcd",x._6)
                                                }
                                        }
                                  }
                                }//case ending

                        }//react ending
                 }
         }

  def checkLeafTable(target:Int) : Int = {
    var nearest = 1000000000;
 // println("reached here..... MaxLeaf : " + maxLeaf + " MinLeaf : " + minLeaf + " Target: " + target + " ForwardLeaf : " + ForwardLeaf)

         i = 0
         var diff = 1000
          while(i<4) { // num of leaf nodes
            if(ForwardLeaf(i) == target){
              return ForwardLeaf(i);
            } else {
                if(math.abs(target - ForwardLeaf(i)) < diff){
                        nearest = (i)
                        diff = math.abs(target - ForwardLeaf(i))
                }
            }
            i += 1
          }
         if (diff >= 2) {
           return -999
         } else {
           return ForwardLeaf(nearest)
         }

    return -999
  }


  def getBinaryOf(x: Int): String = {
   var binarycode = Integer.toBinaryString(x)
          if(binarycode.length()<sizebinary) {
            var diff = sizebinary - binarycode.length();
            while(diff >0){
              binarycode = '0' + binarycode;
              diff -= 1;
            }
          }
   
        return binarycode
  }

  def flipBit(binarycode: String,loc:Int): String = {
        var length = binarycode.length()
        var modBinary:String ="";
        var locFlag = 0
        var inc = 0
        while(inc<length){
                if(loc == inc + 1){
                        if(binarycode(inc) == '1'){
                                modBinary = modBinary + '0'
                        } else {
                                modBinary = modBinary + '1'
                        }
                locFlag = 1
                } else {
                        if(locFlag == 0){
                                modBinary = modBinary + binarycode(inc)
                        } else {
                                modBinary = modBinary + '0'
                        }
                }

                inc += 1
        }
        return modBinary
  }

  def getMatchingPrefix(binarycodeOne: String,binarycodeTwo: String):
Int = { //Assuming strings are of equal length
    var i:Int = 0
    var length:Int = binarycodeOne.length()
    while(i<length){
      try{
      if(binarycodeOne(i) == binarycodeTwo(i)){
        i += 1;
      } else {
        return i
      }
      } catch{
         case e: Exception =>
      }
    }
    return i
  }

  

}