package objsets

import TweetReader.*

/**
 * A class to represent tweets.
 */
class Tweet(val user: String, val text: String, val retweets: Int):
  override def toString: String =
    "User: " + user + "\n" +
    "Text: " + text + " [" + retweets + "]"

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
abstract class TweetSet extends TweetSetInterface:

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet

  /**
   * This method takes a predicate and returns a subset of all the elements
   * in the original set for which the predicate is true.
   *
   * Question: Can we implement this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def filter(p: Tweet => Boolean): TweetSet

  /**
   * Returns a new `TweetSet` that is the union of `TweetSet`s `this` and `that`.
   *
   * Question: Should we implement this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def union(that: TweetSet): TweetSet

  /**
   * Returns the tweet from this set which has the greatest retweet count.
   *
   * Calling `mostRetweeted` on an empty set should throw an exception of
   * type `java.util.NoSuchElementException`.
   *
   * Question: Should we implement this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def mostRetweeted: Tweet

  def descendingByRetweetAcc(acc: TweetList): TweetList

  /**
   * Returns a list containing all tweets of this set, sorted by retweet count
   * in descending order. In other words, the head of the resulting list should
   * have the highest retweet count.
   *
   * Hint: the method `remove` on TweetSet will be very useful.
   * Question: Should we implement this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def descendingByRetweet: TweetList

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

class Empty extends TweetSet:
  override def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = acc
  override def filter(p: Tweet => Boolean): TweetSet = this

  /**
   * The following methods are already implemented
   */

  override def contains(tweet: Tweet): Boolean = false

  override def incl(tweet: Tweet): TweetSet = NonEmpty(tweet, Empty(), Empty())

  override def remove(tweet: Tweet): TweetSet = this

  override def foreach(f: Tweet => Unit): Unit = ()

  override def union(that: TweetSet): TweetSet = that

  override def mostRetweeted: Tweet = throw java.util.NoSuchElementException()

  override def descendingByRetweetAcc(acc: TweetList): TweetList = acc

  override def descendingByRetweet: TweetList = Nil

class NonEmpty(val elem: Tweet, val left: TweetSet, val right: TweetSet) extends TweetSet:

  override def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet =
    val newAcc = if p(elem) then acc.incl(elem) else acc
    left.filterAcc(p, newAcc)
          .union(right.filter(p))

  override def filter(p: Tweet => Boolean): TweetSet =
    filterAcc(p, Empty())


  /**
   * The following methods are already implemented
   */

  override def contains(x: Tweet): Boolean =
    if x.text < elem.text then
      left.contains(x)
    else if elem.text < x.text then
      right.contains(x)
    else true

  override def incl(x: Tweet): TweetSet =
    if x.text < elem.text then
      NonEmpty(elem, left.incl(x), right)
    else if elem.text < x.text then
      NonEmpty(elem, left, right.incl(x))
    else
      this

  override def remove(tw: Tweet): TweetSet =
    if tw.text < elem.text then
      NonEmpty(elem, left.remove(tw), right)
    else if elem.text < tw.text then
      NonEmpty(elem, left, right.remove(tw))
    else
      left.union(right)

  override def foreach(f: Tweet => Unit): Unit =
    f(elem)
    left.foreach(f)
    right.foreach(f)

  override def union(that: TweetSet): TweetSet =
    that match
      case t: Empty => t.union(this)
      case t: NonEmpty =>
        this.left
              .union(this.right.union(t))
              .incl(this.elem)



  override def mostRetweeted: Tweet =
    val leftMost = left match
      case t: Empty => Tweet("", "", -1)
      case t: NonEmpty => t.mostRetweeted

    val rightMost = right match
      case t: Empty => Tweet("", "", -1)
      case t: NonEmpty => t.mostRetweeted

    val sideMost =
      if leftMost.retweets > rightMost.retweets then
        leftMost
      else
        rightMost

    if sideMost.retweets > this.elem.retweets then
      sideMost
    else
      this.elem

  override def descendingByRetweetAcc(acc: TweetList): TweetList =
    val m = this.mostRetweeted
    this.remove(m)
          .descendingByRetweetAcc(acc.append(m))


  override def descendingByRetweet: TweetList =
    descendingByRetweetAcc(Nil)


trait TweetList:
  def head: Tweet
  def tail: TweetList
  def isEmpty: Boolean

  def append(t: Tweet): TweetList

  def foreach(f: Tweet => Unit): Unit =
    if !isEmpty then
      f(head)
      tail.foreach(f)



object Nil extends TweetList:
  override def head = throw java.util.NoSuchElementException("head of EmptyList")
  override def tail = throw java.util.NoSuchElementException("tail of EmptyList")
  override def isEmpty = true

  override def append(t: Tweet): TweetList = Cons(t, Nil)



class Cons(val head: Tweet, val tail: TweetList) extends TweetList:
  override def isEmpty = false

  override def append(t: Tweet): TweetList = Cons(head, tail.append(t))


object GoogleVsApple:
  val google: List[String] = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple: List[String] = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  val all = TweetReader.allTweets

  lazy val googleTweets: TweetSet = all.filter(t => google.exists(str => t.text.contains(str)))
  lazy val appleTweets: TweetSet = all.filter(t => apple.exists(str => t.text.contains(str)))

  /**
   * A list of all tweets mentioning a keyword from either apple or google,
   * sorted by the number of retweets.
   */
  lazy val trending: TweetList =
    googleTweets.union(appleTweets).descendingByRetweet

object Main extends App:
  // Print the trending tweets
  GoogleVsApple.trending foreach println
