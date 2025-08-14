import TestType.Containment

import scala.collection.mutable.ListBuffer
import scala.util.boundary.Label

object ContainmentTest {
  val logger = ListBuffer[String]()

  def isContained(query1: Query, query2: Query): Boolean = {
    logger.clear()
    logger += s"q1 is : ${query1}"
    logger += s"q2 is : ${query2}"
    val q1 = copyQuery(query1, "q1")
    val q2 = copyQuery(query2, "q2")
    val homomorphism = findHomomorphism(q1, q2, Map.empty)

    val res = homomorphism.exists { h =>
      samePredAndArity(q1.head, q2.head) &&
        applyMap(q2.head.terms, h) == q1.head.terms
    }
    setLogger(q1, q2, res, homomorphism)
    writeInFile(generateFileName(Containment, query1.queryId, Some(query2.queryId)), logger.mkString("\n"))
    res
  }

  private def findHomomorphism(q1: Query, q2: Query, h0: Map[String, String]): Option[Map[String, String]] = {
    def dfs(remaining: List[Atom], h: Map[String, String]): Option[Map[String, String]] =
      remaining match {
        case Nil => Some(h)
        case a2 :: rest =>
          val candidates = q1.body.filter(a1 => samePredAndArity(a1, a2))
          candidates.view
            .flatMap(a1 => unify(a1, a2, h)) // Try matching
            .flatMap(h2 => dfs(rest, h2)) // continue with the next atom
            .headOption
      }

    dfs(q2.body.toList, h0)
  }


  private def samePredAndArity(a1: Atom, a2: Atom): Boolean =
    a1.relationName == a2.relationName && a1.terms.size == a2.terms.size

  private def applyMap(terms: List[String], h: Map[String, String]): List[String] =
    terms.map(t => h.getOrElse(t, t))
    
  private def unify(a1: Atom, a2: Atom, h: Map[String, String]): Option[Map[String, String]] = {
    val zipped = a1.terms.zip(a2.terms)
    zipped.foldLeft(Option(h)) {
      case (Some(m), (t1, t2)) =>
        m.get(t2) match {
          case Some(already) =>
            if (already == t1) Some(m) else None
          case None =>
            Some(m + (t2 -> t1))
        }
      case (None, _) => None
    }
  }


  private def copyQuery(query: Query, label: String) : Query = {
    val newHead = Atom(query.head.relationName, query.head.terms.map(t => s"$label.${t}"))
    val newBody = query.body.map(atom => Atom(atom.relationName, atom.terms.map(t => s"$label.${t}")))
    Query(query.queryId, newHead, ListBuffer.from(newBody))
  }


  private def applyMapToTerms(terms: List[String], h: Map[String, String]): List[String] =
    terms.map(t => h.getOrElse(t, t))

  private def applyMapToAtom(a: Atom, h: Map[String, String]): Atom =
    a.copy(terms = applyMapToTerms(a.terms, h))

  private def applyMapToQuery(q: Query, h: Map[String, String]): Query =
    q.copy(
      head = applyMapToAtom(q.head, h),
      body = q.body.map(a => applyMapToAtom(a, h))
    )

  
  private def setLogger(
                         query1: Query,
                         query2: Query,
                         success: Boolean,
                         h: Option[Map[String, String]]
                       ): Unit = {
    if (success) {
      logger += "A possible homomorphism h from q2 to q1 contains the following mappings:"
      h match {
        case Some(map) if map.nonEmpty =>
          logger ++= map.toList.map { case (from, to) => s"$from to $to" }
          val q2h = applyMapToQuery(query2, map)
          logger += s"Then h(q2) is: ${q2h}"
        case Some(map) =>
          logger += "(empty mapping)"
          val q2h = applyMapToQuery(query2, map)
          logger += s"Then h(q2) is: ${q2h}"
        case None =>
          logger += "Logical Error: no homomorphism found, but success is true."
      }
    } else {
      val (db, headTuple) = canonicalDBOf(query1)
      logger += "A possible counterexample database D contains the following atoms:"
      db.foreach(f => logger += f.toString)

      val tup = headTuple.mkString(", ")
      logger += s"Then q1(D) contains the tuple ($tup)."

      val outQ2 = evalQueryOnDB(query2, db)
      if (outQ2.isEmpty)
        logger += s"However, ($tup) is not in q2(D) since q2(D) is empty."
      else {
        logger += s"However, q2(D) is not empty. It contains:"
        outQ2.foreach(t => logger += s"(${t.mkString(", ")})")
      }
    }
  }


  private def evalQueryOnDB(q: Query, db: List[Atom]): List[List[String]] = {
    def unifyWithFact(qAtom: Atom, fact: Atom, env0: Map[String, String]): Option[Map[String, String]] = {
      if (qAtom.relationName != fact.relationName || qAtom.terms.size != fact.terms.size) return None
      qAtom.terms.zip(fact.terms).foldLeft(Option(env0)) {
        case (Some(env), (v, c)) =>
          env.get(v) match {
            case Some(already) if already == c => Some(env)
            case Some(_) => None
            case None => Some(env + (v -> c))
          }
        case (None, _) => None
      }
    }

    def dfs(atoms: List[Atom], env: Map[String, String]): List[Map[String, String]] = atoms match {
      case Nil => List(env)
      case a :: rest =>
        db.flatMap(f => unifyWithFact(a, f, env).toList.flatMap(dfs(rest, _)))
    }

    dfs(q.body.toList, Map.empty).map(env => q.head.terms.map(t => env.getOrElse(t, t)))
  }

  private def canonicalDBOf(q: Query): (List[Atom], List[String]) = {
    val vars = (q.head.terms ++ q.body.flatMap(_.terms)).distinct
    val consts = LazyList.from(0).map(i => s"'${('X' + (i % 26)).toChar}").take(vars.length).toList
    val sigma = vars.zip(consts).toMap
    val db = q.body.map(a => a.copy(terms = a.terms.map(sigma))).toList
    val headT = q.head.terms.map(sigma)
    (db, headT)
  }
}
