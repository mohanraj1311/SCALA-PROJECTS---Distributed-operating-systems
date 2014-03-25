import scala.actors.Actor
import scala.actors._

// case class which accepts the starting number,ending number,fixed number
case class operands(startnum:Int,endnum:Int,fixlen:Int)
//case class result(resultList:List[Int])
//case class res(value:Int)

object Project1 {
	def main(args:Array[String]){
	val masterobj=new masterclass(args(0).toInt,args(1).toInt)
	masterobj.start()
	   }
	}

class masterclass(N:Int,K:Int) extends Actor {
	val totalActors=10;

	def act() {

			var count=1
			var start=1
			//Dividing the range among actors
	
			var end=Math.ceil(N.toDouble/totalActors).toInt;
			var interval=end;

			while(count<=totalActors)
			{
			if(count==totalActors)
			end=N;
		
			//else part
			count=count+1;
			val operand=new operands(start,end,K);
			start=end+1;
			end=end+interval;
			val w=new work;
			w.start
			w!operand
			}


			// Collecting results

			count=1;
			var resultlist=List[Int] ()
			while(count<=totalActors)
			{
				count=count+1;
				receive 
				{
				case res:List[Int]=> resultlist :::=res
				}
			}

			// printing results

			println("The result:"+resultlist.sorted);
				
		}
	}


        // define worker

	class work extends Actor{
		def act()
		{
		react{
			case (operand:operands)=>sender!cal(operand)	

		}
		}
		
		def cal(operand:operands):List[Int] = {
		
			var sum=0.0;
			var sq=0.0;
			val N=operand.endnum
			val k=operand.fixlen
			val st=operand.startnum

			var points=List[Int]()

			//find seq
			for(i:Int<-st to N)
			{
			sum=0.0
			for(j:Int<-i to (i+k-1))
				{
				sum=sum+j.toDouble*j.toDouble
				}
			sq=Math.sqrt(sum);
			if(sq % 1 == 0)
			{
			points::=i
			}
			}
			return points	
		}
	}

	
