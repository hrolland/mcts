package mcts

import scala.util.Random

trait Board {
	val meOrOpponent: Boolean
	val optScoreIfFinal: Option[Int]
	def randomNext(random: Random): Board
	def getAllPossibleNextBoard: Seq[Board]
}

class Node(val board: Board, val parent: Node = null, var subNodes: Seq[Node] = Seq.empty, var nbVisit: Int = 0, var score: Int = 0)

object MonteCarloTreeSearch {
	val random: Random = new Random(System.currentTimeMillis())
	def findNextMove(board: Board, uctsTime: Int): Board = {
		val end = System.currentTimeMillis() + uctsTime
		val rootNode = new Node(board)
		var nbTurn = 0
		while (System.currentTimeMillis() < end) {
			nbTurn += 1
			val promiseNode = selectPromiseNode(rootNode)
			if (promiseNode.subNodes.isEmpty) {
				expandNode(promiseNode)
			}
			var nodeToExplore = promiseNode
			if (promiseNode.subNodes.nonEmpty) {
				nodeToExplore = promiseNode.subNodes(random.nextInt(promiseNode.subNodes.size))
			}
			val playoutResult = simulateRandomPlayout(nodeToExplore)
			backPropogation(nodeToExplore, playoutResult)
		}
		val bestNode = rootNode.subNodes.maxBy(n => n.score.toDouble / n.nbVisit)
		//logTree(rootNode)
		//Console.err.println("rootNode.subNodes :\n\t" + rootNode.subNodes.map(s => (s.score, s.nbVisit)).mkString("\n\t"))
		Console.err.println("found " + bestNode.score + " / " + bestNode.nbVisit + " in " + nbTurn)
		bestNode.board
	}
	def selectPromiseNode(rootNode: Node): Node = {
		var node = rootNode
		while (node.subNodes.nonEmpty) {
			val parentVisitScore = math.log(node.nbVisit)
			node = node.subNodes.maxBy { subNode =>
				if (subNode.nbVisit == 0) {
					Int.MaxValue.toDouble
				} else {
					(1.41D * math.sqrt(parentVisitScore / subNode.nbVisit)) + (subNode.score.toDouble / subNode.nbVisit)
				}
			}
		}
		node
	}
	def expandNode(node: Node): Unit = {
		node.subNodes = node.board.getAllPossibleNextBoard
			.map{ board =>
				new Node(board, node)
			}
	}
	def simulateRandomPlayout(node: Node): Int = {
		if (node.board.optScoreIfFinal.exists(s => s < 0)) {
			node.parent.score = Int.MinValue
			-1
		} else {
			var currentBoard = node.board
			while (currentBoard.optScoreIfFinal.isEmpty) {
				currentBoard = currentBoard.randomNext(random)
			}
			currentBoard.optScoreIfFinal.get
		}
	}
	def backPropogation(node: Node, playout: Int): Unit = {
		var currentNode = node
		while (currentNode != null) {
			currentNode.nbVisit += 1
			if ((playout > 0 && !currentNode.board.meOrOpponent) || (playout < 0 && currentNode.board.meOrOpponent)) {
				if (currentNode.score != Int.MinValue) {
					currentNode.score += 1
				}
			}
			currentNode = currentNode.parent
		}
	}
	private def logTree(node: Node, prefix: String = ""): Unit = {
		Console.err.println(prefix + "(" + node.board.toString + ") " + node.score + "/" + node.nbVisit)
		val newPrefix = prefix + " "
		node.subNodes.foreach(sn => logTree(sn, newPrefix))
	}
}
