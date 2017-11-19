import scala.collection.mutable
import scala.io.Source

object FrequentItemsets {
  type Itemset = Set[Int]

  def main(args: Array[String]): Unit = {
    // Retrieve the data from file and convert to a list of itemsets
    val transactions = Source.fromFile("src/main/resources/chess.dat").getLines
      .map(line => line.split(" ").map(item => item.toInt).toSet)
      .toList

    // Find all the frequent itemsets with support at least minSup
    val minSup = 0.95
    val frequentItemsets = mineFrequentItemsets(transactions, minSup)
    println("==== Frequent itemsets with support at least %.4f ====".format(minSup))
    frequentItemsets.foreach(itemset => println(itemset.mkString(" ")))

    // Find all the association rules with confidence at least minConf
    val minConf = 0.95
    val associationRules = mineAssociationRules(frequentItemsets, transactions, minConf)
    println("==== Assocation rules with support at least %.4f and confidence at least %.4f ====".format(minSup, minConf))
    associationRules.foreach({ case (lhs, rhs) => println("%s => %s".format(lhs.mkString(" "), rhs.mkString(" "))) })

    // Keep only the interesting association rules (those with a non-empty left or right hand side)
    val interestingAccosationRules = associationRules.filter({ case (lhs, rhs) => lhs.nonEmpty && rhs.nonEmpty })
    println("==== Interesting association rules with support at least %.4f and confidence at least %.4f ====".format(minSup, minConf))
    interestingAccosationRules.foreach({ case (lhs, rhs) => println("%s => %s".format(lhs.mkString(" "), rhs.mkString(" "))) })
  }

  def mineFrequentItemsets(transactions: List[Itemset], minSup: Double): Set[Itemset] = {
    val items = transactions.reduce(_ ++ _) // all items
    val s = minSup * transactions.size

    def apriori(lastSets: Set[Itemset], k: Int, accSets: Set[Itemset]): Set[Itemset] = {
      // Generate candidates
      val candidates = lastSets
        .flatMap(set => (items -- set).map(item => set + item))
        .filter(set => set.subsets(k - 1).forall(lastSets.contains))

      // Count occurrences of the candidate itemsets
      val counts = mutable.Map(candidates.map((_, 0)).toSeq: _*) // map where each candidate starts with count 0
      transactions.foreach(set => candidates.foreach(candidate => if (candidate.subsetOf(set)) counts(candidate) += 1))

      // Only keep candidates with count at least s
      val newSets = counts.filter({ case (itemset, count) => count >= s }).keys.toSet

      // Recurse only if there are newly mined itemsets, else return accumulated itemsets
      if (newSets.nonEmpty) apriori(newSets, k + 1, accSets ++ newSets) else accSets
    }

    apriori(items.map(_ => Set[Int]()), 1, Set())
  }

  def mineAssociationRules(itemsets: Set[Itemset], transations: List[Itemset], minConf: Double): Set[(Itemset, Itemset)] = {
    itemsets
      .flatMap(set => set.subsets().map(subset => (subset, set -- subset))) // generate all rules as (lhs, rhs) pairs
      .groupBy(_._1) // group by left hand side
      .flatMap({ case (lhs, rhsGroup) =>
      val countLhs = transations.count(lhs.subsetOf(_)) // count occurrences of left hand side
      rhsGroup.filter({ case (_, rhs) =>
        val lhsRhs = lhs ++ rhs // items both at the left and right hand side
      val countLhsRhs = transations.count(lhsRhs.subsetOf(_)) // count occurrences of both sides
        countLhsRhs.toDouble / countLhs >= minConf // only keep (lhs, rhs) pairs with confidence at least minConf
      })
    })
      .toSet
  }
}
