use chumsky::Parser;
use std::fmt;
use std::ops::{Deref, DerefMut};
use wacc_rs::parser::parser;
use wacc_rs::token::lexer;

const TEST_EXPR: &str = r#"
(foo == bar[23][3234 + ord - chr flll][34][234]) * len - ord ("some string literal" - chr - +2341) >= 23 == '\\'
"#;

const TEST_TYPE: &str = r#"
pair(int, pair(pair, string)[][][])[][]
"#;

const TEST_PROGRAM: &str = r#"
# The program reads n (number of integers), then n integers. After each input, 
# it insert the integer into a binary search tree. At the end, it prints out 
# the content in the binary search tree so that we have a sorted list of 
# integer.
# 
# We represent a node in the binary search tree using two pair elements. The 
# first element has a type <int, pair>, the int is the integer stored in the 
# node, the pair is the pointer to the second pair element. The second pair 
# element has a type <pair, pair> which is the pointer to the two children 
# nodes in the binary search tree.

begin

  # Create a new node of a binary search tree having the given integer value 
  # and points to the two given pairs.
  pair(int, pair) createNewNode(int value, pair(int, pair) left, pair(int, pair) right) is
    pair(pair, pair) p = newpair(left, right) ;
    pair(int, pair) q = newpair(value, p) ;
    return q
  end

  # Given a root of a binary search tree and an integer to insert, the function 
  # inserts the integer into the tree and returns the new root of the tree.
  pair(int, pair) insert(pair(int, pair) root, int n) is
    if root == null then
      root = call createNewNode(n, null, null)
    else
      pair(pair, pair) p = snd root ;
      int current = fst root ;
      pair(int, pair) q = null ;
      if n < current then
      	q = fst p ;
        fst p = call insert(q, n)
      else 
      	q = snd p ;
        snd p = call insert(q, n)
      fi 
    fi ;
    return root
  end

  # Print the integers in the binary search tree in the increasing order.
  int printTree(pair(int, pair) root) is
    if root == null then
      return 0 
    else
      pair(pair, pair) body = snd root ;
      pair(int, pair) p = fst body ;
      int temp = call printTree(p) ;
      temp = fst root ; 
      print temp ;
      print ' ' ;
      p = snd body ;
      temp = call printTree(p) ;
      return 0
    fi
  end

  # The main function
  int n = 0 ;
  print "Please enter the number of integers to insert: " ;
  read n ;
  print "There are " ;
  print n ;
  println " integers." ;
  int i = 0 ;
  pair(int, pair) root = null ;
  while i < n do
    int x = 0 ;
    print "Please enter the number at position " ; 
    print i + 1 ;
    print " : " ;
    read x ;
    root = call insert(root, x) ;
    i = i + 1
  done ;
  print "Here are the numbers sorted: " ;
  i = call printTree(root) ;
  println ""
end
"#;

fn main() {
    let (tokens, parse_errs) = lexer().parse_recovery_verbose(TEST_PROGRAM);

    if let Some(tokens) = tokens {
        println!("{}", DisplayVec(tokens.clone()));

        let (parsed, parse_errs) = parser()
            .parse_recovery_verbose(tokens.iter().map(|s| s.inner.clone()).collect::<Vec<_>>());

        if let Some(parsed) = parsed {
            println!("{}", parsed);
        }

        parse_errs
            .into_iter()
            .for_each(|e| println!("Parse error: {}", e))
    }

    parse_errs
        .into_iter()
        .for_each(|e| println!("Parse error: {}", e))
}

#[repr(transparent)]
struct DisplayVec<T>(Vec<T>);

impl<T> DisplayVec<T> {
    const DISPLAY_WIDTH: usize = 4;
    const OFFSET_WIDTH: usize = 2;
}

impl<T> Deref for DisplayVec<T> {
    type Target = Vec<T>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for DisplayVec<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<T: fmt::Display> fmt::Display for DisplayVec<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{{")?;

        let mut width = 0;
        for i in self.iter() {
            // control items-per-line width
            if width == 0 {
                write!(f, "\n")?;
                for _ in 0..DisplayVec::<T>::OFFSET_WIDTH {
                    write!(f, " ")?;
                }
                width = DisplayVec::<T>::DISPLAY_WIDTH;
            }
            width -= 1;
            write!(f, "{}, ", i)?;
        }

        write!(f, "\n}}")
    }
}
