package objsets

import common._
import TweetReader._

/**
 * A class to represent tweets.
 */
class Tweet(val user: String, val text: String, val retweets: Int) {
  override def toString: String =
    "User: " + user + "\n" +
    "Text: " + text + " [" + retweets + "]"
}

/**
 * This represents a set of objects of type `Tweet` in the form of a binary search
 * tree. Every branch in the tree has two children (two `TweetSet`s). There is an
 * invariant which always holds: for every branch `b`, all elements in the left
 * subtree are smaller than the tweet at `b`. The elements in the right subtree are
 * larger.
 *
 * Note that the above structure requires us to be able to compare two tweets (we
 * need to be able to say which of two tweets is larger, or if they are equal). In
 * this implementation, the equality / order of tweets is based on the tweet's text
 * (see `def incl`). Hence, a `TweetSet` could not contain two tweets with the same
 * text from different users.
 *
 *
 * The advantage of representing sets as binary search trees is that the elements
 * of the set can be found quickly. If you want to learn more you can take a look
 * at the Wikipedia page [1], but this is not necessary in order to solve this
 * assignment.
 *
 * [1] http://en.wikipedia.org/wiki/Binary_search_tree
 */
abstract class TweetSet {
  def elem: Tweet
  def left: TweetSet
  def right: TweetSet
  
  def isEmpty: Boolean
  /**
   * This method takes a predicate and returns a subset of all the elements
   * in the original set for which the predicate is true.
   *
   * Question: Can we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def filter(p: Tweet => Boolean): TweetSet =  filterAcc(p, new Empty())


  
  /**
   * This is a helper method for `filter` that propagetes the accumulated tweets.
   */
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = acc

  /**
   * Returns a new `TweetSet` that is the union of `TweetSet`s `this` and `that`.
   *
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def union(that: TweetSet): TweetSet
   

  /**
   * Returns the tweet from this set which has the greatest retweet count.
   *
   * Calling `mostRetweeted` on an empty set should throw an exception of
   * type `java.util.NoSuchElementException`.
   *
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def mostRetweeted: Tweet

  def leastRetweeted: Tweet
  /**
   * Returns a list containing all tweets of this set, sorted by retweet count
   * in descending order. In other words, the head of the resulting list should
   * have the highest retweet count.
   *
   * Hint: the method `remove` on TweetSet will be very useful.
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def descendingByRetweet: TweetList
  def ascendingByRetweet: TweetList

  /**
   * The following methods are already implemented
   */

  /**
   * Returns a new `TweetSet` which contains all elements of this set, and the
   * the new element `tweet` in case it does not already exist in this set.
   *
   * If `this.contains(tweet)`, the current set is returned.
   */
  def incl(tweet: Tweet): TweetSet

  /**
   * Returns a new `TweetSet` which excludes `tweet`.
   */
  def remove(tweet: Tweet): TweetSet

  /**
   * Tests if `tweet` exists in this `TweetSet`.
   */
  def contains(tweet: Tweet): Boolean

  /**
   * This method takes a function and applies it to every element in the set.
   */
  def foreach(f: Tweet => Unit): Unit
  
  def size() : Int
}

class Empty extends TweetSet {
  def size() : Int = 0
  override def elem: Tweet = null
  override def left: TweetSet = null
  override def right: TweetSet = null
  
  def isEmpty: Boolean = true
  
  override def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = acc

  def mostRetweeted: Tweet = throw new Exception("java.util.NoSuchElementException")

  def leastRetweeted: Tweet = throw new Exception("java.util.NoSuchElementException")

  def descendingByRetweet: TweetList = Nil
  def ascendingByRetweet: TweetList = Nil
  
  def union(that: TweetSet): TweetSet = that
  
  /**
   * The following methods are already implemented
   */

  
  def contains(tweet: Tweet): Boolean = false

  def incl(tweet: Tweet): TweetSet = new NonEmpty(tweet, new Empty, new Empty)

  def remove(tweet: Tweet): TweetSet = this

  def foreach(f: Tweet => Unit): Unit = ()
}

class NonEmpty(el: Tweet, l: TweetSet, r: TweetSet) extends TweetSet {
  def isEmpty: Boolean = false
  override def elem: Tweet = el
  override def left: TweetSet = l
  override def right: TweetSet = r
  
  override def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = {
    var temp = acc
    if(p(elem)) 
      temp = temp.incl(elem)
    temp = left.filterAcc(p, temp)
    temp = right.filterAcc(p, temp)
    temp
  }

  def mostRetweeted: Tweet = {
    var max = elem
    var maxLeft:Tweet = max
    var maxRight:Tweet = max
    if(!left.isEmpty)
      maxLeft = left.mostRetweeted
    if(!right.isEmpty)
      maxRight = right.mostRetweeted
   
    if(max.retweets < maxLeft.retweets)
      max = maxLeft
    if(max.retweets < maxRight.retweets)
      max = maxRight
    
    max
  }

  def leastRetweeted: Tweet = {
    var min = elem
    var minLeft:Tweet = min
    var minRight:Tweet = min
    if(!left.isEmpty)
      minLeft = left.leastRetweeted
    if(!right.isEmpty)
      minRight = right.leastRetweeted
   
    if(min.retweets > minLeft.retweets)
      min = minLeft
    if(min.retweets > minRight.retweets)
      min = minRight
    
    min
  }
    
  def descendingByRetweet: TweetList = {
    var sorted:TweetList = Nil
    var s:TweetSet = this
    while(!s.isEmpty){
      var min = s.leastRetweeted
      sorted = new Cons(min, sorted)
      s = s.remove(min)
    }
    
    sorted
  }
  
  def ascendingByRetweet: TweetList = {
    var sorted:TweetList = Nil
    var s:TweetSet = this
    var counter:Int = 0
    
    while(!s.isEmpty){
      var max = s.mostRetweeted
      sorted = new Cons(max, sorted)     
      s = s.remove(max)
    }
    
    sorted
  }
  
  def union(that: TweetSet): TweetSet = {
    if(that.isEmpty) this 
    else{
      var s : TweetSet = this.incl(that.elem)
      s = s.union(that.left)
      s = s.union(that.right)
      s
    }
    
  }
  
  def size() : Int = 1 + left.size() + right.size()
  
  /**
   * The following methods are already implemented
   */
  
  def contains(x: Tweet): Boolean =
    if (x.text < elem.text) left.contains(x)
    else if (elem.text < x.text) right.contains(x)
    else true

  def incl(x: Tweet): TweetSet = {
    if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)
    else if (elem.text < x.text) new NonEmpty(elem, left, right.incl(x))
    else this
  }

  def remove(tw: Tweet): TweetSet =
    if (tw.text.compareTo(elem.text) < 0 ) new NonEmpty(elem, left.remove(tw), right)
    else if (elem.text.compare(tw.text) < 0) new NonEmpty(elem, left, right.remove(tw))
    else {
      left.union(right)
    }
  
  def foreach(f: Tweet => Unit): Unit = {
    f(elem)
    left.foreach(f)
    right.foreach(f)
  }
}

trait TweetList {
  def head: Tweet
  def tail: TweetList
  def isEmpty: Boolean
  def foreach(f: Tweet => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }
}

object Nil extends TweetList {
  def head = throw new java.util.NoSuchElementException("head of EmptyList")
  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")
  def isEmpty = true
}

class Cons(val head: Tweet, val tail: TweetList) extends TweetList {
  def isEmpty = false
}


object GoogleVsApple {
  //val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  //val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  val google = List("android", "Android")
  val apple = List("ios", "iOS")
  
  lazy val googleTweets: TweetSet = TweetReader.allTweets.filter(p => google.exists(s => p.text.contains(s)))
  lazy val appleTweets: TweetSet = TweetReader.allTweets.filter(p => apple.exists(s => p.text.contains(s)))

  /**
   * A list of all tweets mentioning a keyword from either apple or google,
   * sorted by the number of retweets.
   */

  lazy val trending: TweetList = googleTweets.union(appleTweets).ascendingByRetweet
  lazy val trending2: TweetList = googleTweets.union(appleTweets).descendingByRetweet
}

object Main extends App {
  // Print the trending tweets
  
  
  GoogleVsApple.trending foreach println
  //GoogleVsApple.trending2 foreach println
}
