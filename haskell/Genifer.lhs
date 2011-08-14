\documentclass[12pt, a4paper]{report}

%dvips OR pdftex
\usepackage[pdftex, pdfauthor={Yan King Yin}, bookmarks, anchorcolor=blue, colorlinks, linkcolor=blue, citecolor=blue, urlcolor=blue, breaklinks, plainpages=false, pdfpagelabels, hypertexnames=false]{hyperref}
\usepackage{breakurl}
\usepackage[final]{graphicx}
\usepackage{float}
\usepackage[square]{natbib}
\usepackage{color}
\usepackage{amsmath}     % for {equation} {cases} {xleftarrow}
\usepackage{bbm}         % for math fonts with double lines
\usepackage{amssymb}     % for \curlyvee
\usepackage{stmaryrd}    % for \bigcurlyvee, \widetilde{\bigwedge}
\usepackage{wasysym}     % for \iint (double integral)
\usepackage{makeidx}
\usepackage[style=list,acronym=true,number=none]{glossary}
\usepackage{alg}
\usepackage{minitoc}
\usepackage[pointedenum]{paralist}
\usepackage[sf,bf,big,raggedright,compact]{titlesec}
\usepackage{fancyhdr}
\usepackage{movie15}
\usepackage{caption}

\newcommand{\formula}[1]{\textcolor{PineGreen}{#1}}
\newcommand{\english}[1]{\rmfamily \textit{#1} \sffamily}
\renewcommand{\star}{$* \;$}
\newcommand{\app}{$\cdot \;$}
\newcommand{\eg}{\textbf{Example:} }
\newcommand{\tab}{\hspace*{1cm} }
\newcommand{\tv}[1]{$\langle${#1}$\rangle$}
\newcommand{\underconst}{\includegraphics{UnderConst.png}}
\renewcommand{\vec}[1]{\mathbf{#1}}

%%%%%%%%%%%%%%%%% Symbols defined by me %%%%%%%%%%%%%%%%%%%
\newcommand{\defeq}{\stackrel{def}{=}}
% fuzzy and probabilistic logic operators
\newcommand{\Zand}{\stackrel{Z}{\wedge}}
\newcommand{\Zor}{\stackrel{Z}{\vee}}
\newcommand{\Zneg}{\stackrel{Z}{\neg}}
\newcommand{\Pand}{\stackrel{P}{\wedge}}
\newcommand{\Por}{\stackrel{P}{\vee}}
\newcommand{\bigZand}{\stackrel{Z}{\bigwedge}}
\newcommand{\bigZor}{\stackrel{Z}{\bigvee}}
\newcommand{\bigPand}{\stackrel{P}{\bigwedge}}
\newcommand{\bigPor}{\stackrel{P}{\bigvee}}
\newcommand{\Pandor}{\stackrel{P}{\varodot}}
\newcommand{\Zandor}{\stackrel{Z}{\varodot}}
\newcommand{\bigPandor}{\stackrel{P}{\bigodot}}
\newcommand{\bigZandor}{\stackrel{Z}{\bigodot}}
\newcommand{\Pimp}{\rightarrowtriangle}       % probabilistic implication ->
\newcommand{\PimpL}{\leftarrowtriangle}       % probabilistic implication <-
% categories
\newcommand{\catP}{\mathcal{P}}
\newcommand{\catZ}{\mathcal{Z}}
\newcommand{\catB}{\mathcal{B}}
\newcommand{\catC}{\mathcal{C}}
\newcommand{\catPB}{\mathcal{P(B)}}
\newcommand{\catPZ}{\mathcal{P(Z)}}
\newcommand{\setN}{\mathbb{N}}
\newcommand{\setR}{\mathbb{R}}
\newcommand{\Hyp}{\mathbbm{Hyp}}

\fontsize{13pt}{15pt} \selectfont
\renewcommand{\familydefault}{\sfdefault}
%\renewcommand{\mtcfont}{\small\sffamily}
%\renewcommand{\mtcSfont}{\small\sffamily}
%\renewcommand{\mtcSSfont}{\small\sffamily}
%\renewcommand{\mtcSSSfont}{\small\sffamily}
%\renewcommand{\mtifont}{\small\sffamily}
\renewcommand{\mtcfont}{\normalsize\sffamily}
\renewcommand{\mtcSfont}{\normalsize\sffamily}
\renewcommand{\mtcSSfont}{\normalsize\sffamily}
\renewcommand{\mtcSSSfont}{\normalsize\sffamily}
\renewcommand{\mtifont}{\normalsize\sffamily}
%\renewcommand{\mtcfont}{\large\sffamily}
%\renewcommand{\mtcSfont}{\large\sffamily}
%\renewcommand{\mtcSSfont}{\large\sffamily}
%\renewcommand{\mtcSSSfont}{\large\sffamily}
%\renewcommand{\mtifont}{\large\sffamily}
\renewcommand{\mtctitle}{}
\renewcommand{\chaptername}{Ch }

\titlespacing{\chapter}{0pt}{0pt}{0pt}
\titleformat{\chapter}[hang]{\sffamily\bfseries\Huge\color{blue}}{\thechapter \hspace{30pt}}{0pt}{}
\titleformat{\appendix}[hang]{\sffamily\bfseries\Huge\color{blue}}{\thechapter \hspace{30pt}}{0pt}{}
\titleformat{\section}[hang]{\sffamily\bfseries\Large\color{blue}}{\thesection \hspace{10pt}}{0pt}{}
\titleformat{\subsection}[hang]{\sffamily\bfseries\large\color{blue}}{\thesubsection \hspace{5pt}}{0pt}{}
\titleformat{\subsubsection}[hang]{\sffamily\bfseries\color{blue}}{\thesubsubsection \hspace{5pt}}{0pt}{}

\newcommand{\algrepeatforever}{\textbf{repeat forever}\\\algbegin}

\interfootnotelinepenalty=50000

\title{\textbf{Genifer in Haskell}}
\author{Contributors: \\
Abram Demski \\
Seh \\
% Sandeep Pai \\
William Taysom \\
YKY (general.intelligence@@Gmail.com) }
\date{\copyright \quad latest revision: \today}

\setlength{\headheight}{0cm}
\setlength{\hoffset}{0cm}
\setlength{\topmargin}{-2cm}
\setlength{\oddsidemargin}{-2cm}
\setlength{\evensidemargin}{-2cm}
\setlength{\textwidth}{19.5cm}
\setlength{\textheight}{28cm}
\setlength{\headsep}{0cm}
\setlength{\topskip}{0cm}
\setlength{\footskip}{0.9cm}  % between bottom of page and page number
\setlength{\floatsep}{0cm}
\setlength{\textfloatsep}{0.6cm}
\setlength{\intextsep}{0.5cm}
\setlength{\parindent}{0em}
\setlength{\parskip}{7pt}
\setlength{\pltopsep}{-5pt}   % for compact-enum

\pagestyle{fancy}
\renewcommand{\headrulewidth}{0pt}
\rhead{}
\lhead{}
\cfoot{}
\rfoot{\thepage}
\fancypagestyle{plain}{\rfoot{\thepage}}
%\pagestyle{plain}

%include polycode.fmt
\begin{document}
\renewcommand{\normalsize}{\fontsize{13pt}{15pt}\selectfont}
\fontsize{13pt}{15pt} \selectfont
\framedhs

\maketitle
\dominitoc
\tableofcontents

\chapter{Preface}

\section{Literate programming}

This file is Genifer.pdf, generated from Genifer.lhs by:\\
\tab lhs2tex -o Genifer.tex Genifer.lhs\\
\tab pdflatex Genifer.tex

You can run the source code contained in Genifer.lhs by:\\
\tab ghci Genifer.lhs

\chapter{Main}

\begin{code}
{-# LANGUAGE NoMonomorphismRestriction #-} -- Otherwise we need more type declarations.

import Data.Char
import Data.List
import Data.Maybe
import Data.Either
import Text.Parsec

main = do
       putStrLn "Hello, World!"
       -- Tests --
       print prop_showTerm
       print prop_parseTerm_success
       print prop_parseTerm_fail
\end{code}

\chapter{Predicate logic}

\section{Data structures for logic}

The definition of types and terms are pretty standard for $\lambda$-calculus:

\subsection{Types}

The set of types is the closure of: \\
$\alpha_1, \alpha_2, ..., \alpha_n \in T $ and \\
$\beta \in T_0 \Rightarrow (\alpha_1 \times \alpha_2 \times ... \times \alpha_n \rightarrow \beta) \in T$

\subsubsection{Order of a type}

The order of a type is defined by:\\
-- if $\tau \in T_0 \quad Ord(\tau) = 1$ \\
-- if $\tau = (\alpha_1 \times \alpha_2 \times ... \times \alpha_n \rightarrow \beta) \quad
   Ord(\tau) = max\{ Ord(\alpha_i) \mid 1 \le i \le n \} + 1 $

\subsection{Terms}

-- $\mathcal{C}$ is the set of \textbf{constant} symbols. \\
-- $\mathcal{V}$ is the set of \textbf{variable} symbols.

\begin{figure}[h]
\centering
\includegraphics{HOL-syntax.png}
\caption{HOL syntax}
% \label{fig:HOL-syntax}
\end{figure}

\begin{code}
type Id = (Int, String)
data Var = Var Id                           deriving (Show, Eq)
data Const = Const Id                       deriving (Show, Eq)

data Term =
       VarAtom Var
     | ConstAtom Const
     | ApplyVar Var [Term]
     | ApplyConst Const [Term]
     | Lambda [Var] Term
  deriving (Show, Eq)
\end{code}

% This is a test:
% 
% \begin{code}
% size :: Term -> Int
% size (AtomicTerm a) = 1
% size (Apply u v) = 1 + (sum (map size v))
% size (Lambda u v) = (length u) + 1
% \end{code}

% \section{First-order unification}
% 
% The following code is borrowed from Takashi Yamamiya's web site:\\
% \tab http://propella.blogspot.com/2009/04/prolog-in-haskell.html
% 
% The type:
% \begin{spec}
% type Substitution = [(Term, Term)]
% true = []
% \end{spec}
% 
% This is the code for applying a substitution:
% \begin{spec}
% -- apply [(w"X", w"Y"), (w"Y", w"Z")] [(w"X"), (w"Y")] == [(w"Z"), (w"Z")]
% apply :: Substitution -> [Term] -> [Term]
% apply s ts = [applyTerm s t | t <- ts]
% 
% applyTerm [] (Var y n)                                  = Var y n
% applyTerm ((Var x i, t):s) (Var y j) | x == y && i == j = applyTerm s t
%                                      | otherwise        = applyTerm s (Var y j)
% applyTerm s (Struct n ts)                               = Struct n (apply s ts)
% \end{spec}
% 
% And here is unify:
% \begin{spec}
% -- unify (w"X") (w"apple") == Just [(w"X", w"apple")]
% unify :: Term -> Term -> Maybe Substitution
% unify (Var x n) (Var y m) = Just [(Var x n, Var y m)]
% unify (Var x n)      y    = Just [(Var x n,       y)]
% unify      x    (Var y m) = Just [(Var y m,       x)]
% unify (Struct a xs) (Struct b ys)
%       | a == b = unifyList xs ys
%       | otherwise   = Nothing
% 
% unifyList :: [Term] -> [Term] -> Maybe Substitution
% unifyList [] [] = Just true
% unifyList [] _ = Nothing
% unifyList _ [] = Nothing
% unifyList (x:xs) (y:ys) = do s <- unify x y
%                              s' <- unifyList (apply s xs) (apply s ys)
%                              return (s ++ s')
% \end{spec}

\section{Second-order unification}

This algorithm is from \citep*{Huet1978}, which is a restriction of \citep*{Huet1975} to the
2nd-order case.  It has 2 parts:  Simplfy and GrowTree.

\subsection{Simplify}

The first part simplifies a set $N$ of pairs of terms, by recognizing common constant initial
subterms.  $\mathcal{C}$ is the set of constants.

\begin{algorithm}[H]
\caption{Simplify}
\alginout{$N$ a set of pairs of terms}
{new $N$}
\begin{algtab}
\alglabel{alg:SO-unify-simplify}

\algrepeat
if $N = \emptyset$ then exit with answer \fbox{S} \\

%\addtocounter{algline}{-1}\algnonumber

\algif{there is no pair \tv{$t_1, t_2$} in $N$ with $t_1$ rigid}

 exit with the answer set $N$.  \\

\algelse{select a pair of the form $\langle \lambda x_1 ... x_n \cdot \phi (t_1, ..., t_p), \;
      \lambda x'_1 ... x'_n \cdot \phi' (t'_1, ..., t'_p) \rangle $ \\
where $\phi \in \{ x_1, ..., x_n \} \cup \mathcal{C}$ } \\

\algif{$\phi'$ is $\phi$ modulo bound variable renaming, ie, \\
       $\phi' = \phi$ when $\phi \in \mathcal{C}$, and $\phi' = x'_i$ when $\phi = x_i$ }

replace the pair with $ \{ \langle \lambda x_1 ... x_n \cdot t_i,  \;
                                            \lambda x'_1 ... x'_n \cdot t'_i \rangle \; \mid
                                            1 \le i \le p \} $ \\

\algelse{exit with answer \fbox{F}} \\

\end{algtab}
\end{algorithm}
\vspace{-0.6cm}

\begin{code}
success = True
failure = False

simplify :: Either Bool [(Term,Term)] -> Either Bool [(Term,Term)]
simplify (Right []) = Left success

simplify (Right (x:xs)) = let (t1,t2) = x in
    if (rigid t1) then
        if (renames t1 t2) then Right ((decompose t1 t2) ++ (fromRight (simplify (Right xs))))
        else Left failure -- TODO1
    else simplify (Right xs)

fromRight :: Either a b -> b
fromRight (Right r) = r

rigid :: Term -> Bool
rigid (ConstAtom _) = True
rigid (ApplyConst _ _) = True
rigid (Lambda _  (ApplyConst _ _)) = True
rigid (Lambda vs (ApplyVar v _)) = v `elem` vs
rigid _ = False

renames :: Term -> Term -> Bool
renames t1 t2 = let h1 = (head_of t1)
                    h2 = (head_of t2) in
                if (isConstant h1) then
                   h1 == h2
                else let (Right hh1) = h1
                         (Right hh2) = h2
                         i = (elemIndex hh1 (boundVars t1))
                     in (not (isNothing i)) && (i == (elemIndex hh2 (boundVars t2)))

head_of :: Term -> Either Const Var
head_of (VarAtom v)                 = Right v
head_of (ConstAtom c)               = Left  c
head_of (ApplyVar v _)              = Right v
head_of (ApplyConst c _)            = Left  c
head_of (Lambda _ (VarAtom v))      = Right v
head_of (Lambda _ (ConstAtom c))    = Left  c
head_of (Lambda _ (ApplyVar v _))   = Right v
head_of (Lambda _ (ApplyConst c _)) = Left  c

isConstant :: Either Const Var -> Bool
isConstant (Left _) = True

boundVars :: Term -> [Var]
boundVars (Lambda vs _) = vs
boundVars _ = []

decompose :: Term -> Term -> [(Term,Term)]
-- for all vars in arguments of t1, generate the pair:
--           < Lambda (boundVars t1) t_i, Lambda (boundVars t2) t_i2 >
-- on entry t1 is known to be rigid, and t2 renames t1
decompose (ConstAtom _) _ = []          -- not sure if this is needed
decompose (ApplyConst _ ts) (ApplyConst _ ts2) =  zip ts ts2
decompose (Lambda vs (ApplyConst _ ts)) (Lambda vs2 (ApplyConst _ ts2))
    = zip (map (\t -> (Lambda vs  t)) ts)
          (map (\t -> (Lambda vs2 t)) ts2)
decompose (Lambda vs (ApplyVar _ ts)) (Lambda vs2 (ApplyVar _ ts2))
    = zip (map (\t -> (Lambda vs  t)) ts)
          (map (\t -> (Lambda vs2 t)) ts2)
\end{code}

\subsection{Growing the Matchings Tree}

To grow the Matchings Tree:

\begin{compactenum}
\item The root node is $N_0 = Simplify \{ \langle t, t' \rangle \}$.

\item Nodes \fbox{S} and \fbox{F} are leaves. The successors of a non-leave node $N$ is
  constructed by first selecting from $N$ an arbitrary pair \tv{$t_1, t_2$}. Then then function
  $Match(t_1,t_2)$ below returns a list of substitutions $\{\sigma_1, ... ,\sigma_k\}$.
  For each substitution, grow an arc labelled with $\sigma_i$ from N to its successor
  $Simplfy(\sigma_i N)$.  (If $k=0$, replace $N$ by $\fbox{F}$.)
\end{compactenum}

\begin{algorithm}[H]
\caption{Match}
\alginout{$\tau(t_1)=\tau(t_2)$, $t_2$ is rigid, $t_1$ is not}
{a list of substitutions $\sigma_1,...,\sigma_k$}
\begin{algtab}
\alglabel{alg:SO-unify-match}

\algswitch{order of the head $\mathcal{H}(t_1)$ }

\algcase {order = 1}
\algbegin
$t_1 = \lambda u_1 ... u_n \cdot x \quad \tau(x) \in T_0 $ \\
Let $t_2 = \lambda v_1 ... v_n \cdot t'_2$ \\
\algifthenelse{one of the $v$'s appear free in $t'_2$}
{$k = 0$, no solutions}
{$k=1$, return the unique solution $\langle x, t'_2 \rangle$}
\algend
\algcase {order = 2}
\algbegin
  $t_1 = \lambda u_1 ... u_n \cdot f(t^1_1,...,t^p_1)$ \\
  $t_2 = \lambda v_1 ... v_n \cdot \phi(t^1_2,...,t^q_2)$ \quad with \quad
         $\phi \in \{ v_1,...,v_n \} \cup \mathcal{C} $ \\
  generate $k \le p+1$ solutions as follows: \\

  \algif{$\phi \in \mathcal{C}$}
  \algbegin
    generate one \textbf{imitation} $\langle f, \lambda x_1,...,x_p \cdot \phi(\hat{t}_1,...,\hat{t}_q) \rangle$ \\
    where $\hat{t}_i$ is constructed as: \\
    \algif{$\tau(t^i_2) \in T_0$}
       $\hat{t}_i = h_i(x_1,...,x_p)$ \\
    \algend
    \algif{$\tau(t^i_2) = (\alpha_1 \times ... \times \alpha_s \rightarrow \beta)$}
        $\hat{t}_i = \lambda w_1 ... w_s \cdot h_i(x_1,...,x_p, w_1,...,w_s)$ \\ 
        \quad \quad with $\tau(w_j) = \alpha_j \quad 1 \le j \le s$ \\
        \quad \quad where $h_i$'s are new distinct variables of the appropriate type \\
    \algend
  \algend
  
  \algelse{generate all the \textbf{projections} $\langle f, \lambda x_1,...,x_p \cdot x_j \rangle$
    that are type compatible \\
    ie, such that $\tau(f) = (\gamma_1 \times ... \times \gamma_p \rightarrow \delta)$
        with $\delta = \gamma_j$ } \\
\algend

\end{algtab}
\end{algorithm}
\vspace{-0.6cm}

\section{Substitution management}

\chapter{Message passing}

The agent waits for messages.  An incoming message can be:
\begin{compactenum}
\item a goal
\item an answer
\end{compactenum}

Perhaps the format can be:
\begin{compactenum}
\item goal:     "G formula"
\item answer:   "A goal-ID value"
\end{compactenum}

\section{Processing goals}

Try to match the goal against rules in the (local) KB.

If there is a match, apply the rule, ie, instantiate it with appropriate substitutions, get the subgoals.

Check if the subgoals can be answered by the local KB.  If yes, recurse.\\
If no, send the subgoal to the \textbf{Priority Queue}, ie, certain agent(s) in the network responsible for routing new goals.

\section{Processing answers}

Locate the instantiated rule in the "Instance Store".

Attach the answer to the instantiated rule.

If there are answers for all the subgoals in that rule, evaluate it (see \S\ref{ch:tv-evaluation} TV evaluation).

\section{ZeroMQ}

% \section{CloudHaskell}
% 
% \begin{code}
% import Remote
% 
% import Data.Typeable (Typeable)
% import Data.Data (Data)
% import Data.Binary (Binary,get,put)
% import System.Random (randomR,getStdRandom)
% import Control.Concurrent (threadDelay)
% import Control.Monad (when)
% import Control.Monad.Trans (liftIO)
% 
% main = putStrLn "Hello, World!"
% \end{code}

\chapter{Uncertainty}
\label{ch:uncertainty}

\section{Combining multiple rules}

If all rules are aligned in the ``forward'' direction, as in:
\begin{eqnarray}
H_1 &\PimpL& A_1, A_2, A_3, ... \nonumber \\
H_2 &\PimpL& B_1, B_2, B_3, ... \nonumber \\
    & ... &                     \nonumber
\end{eqnarray}
apply Abram's technique:
\begin{eqnarray}
P(H||A_i,B_i,...) &=& \frac{ P(A_i,B_i,...||X)P(X) } { P(A_i,B_i,...) } \tab\tab\tab \mbox{-- Bayes law} \nonumber \\
 &=& \frac{ P(A_i||X) P(B_i||X) ... P(X) }{ P(A_i,B_i,...) } \tab\tab \mbox{-- Naive Bayes assumption} \nonumber \\
 &=& \frac{ P(H_1||A_i) P(A_i) P(H_2||B_i) P(B_i) ... }{ P(A_i,B_i,...) P(X)^{n-1} } \tab \mbox{-- Bayes law again} \nonumber
\end{eqnarray}

If any of the rules is malaligned, we need to reverse that rule first, by Bayes law:
$$ P(B^*||H,B_{i \neq *}) = ?  $$

\section{Evaluating a single rule}

FUNCTION: Given a rule, its arguments and parameters, and messages attached to each argument, calculate the resulting message.

INPUT:\\
\begin{tabular}{lll}
op      &=& operator of rule\\
head    &=& list of head literals\\
body    &=& list of body literals\\
subgoal &=& which argument is the subgoal\\
params  &=& list of parameters, one element for each body literal\\
msgsH   &=& list of messages for head literals\\
msgsB   &=& list of messages for body literals
\end{tabular}

At this stage we assume the operator is always (probabilistic) "AND".

Algorithm:\\
Case 1:  If subgoal = one of the heads, apply rule directly.\\
Case 2:  If subgoal = one of the body literals, apply rule in reverse (via Bayes law).

\begin{code}
evalRule op head body subgoal params msgsH msgsB
    | subgoal >= 0 = sum (zipWith (*) params msgsB)
    | otherwise    = 0  -- TODO
\end{code}

%\chapter{Memory}

%\chapter{Induction}

\chapter{Miscellaneous functions}

\section{Pretty printing}

\begin{code}
showVar   (Var   (_, s)) = s
showConst (Const (_, s)) = s

showTerm (VarAtom   (Var (_, s))) = s
showTerm (ConstAtom (Const (_, s))) = s
showTerm (ApplyConst c ts) = showConst c ++ "(" ++ intercalate ", " (map showTerm ts) ++ ")"
showTerm (ApplyVar   v ts) = showVar   v ++ "(" ++ intercalate ", " (map showTerm ts) ++ ")"
showTerm (Lambda vs t) = "\\" ++ intercalate " " (map showVar vs) ++ " . " ++ showTerm t

-- Test --

lxy_Pxy = Lambda [Var (0, "x"), Var (0, "y")]
  (ApplyConst (Const (0, "P")) [VarAtom (Var (0, "x")), VarAtom (Var (0, "y"))])

prop_showTerm = "\\x y . P(x, y)" == showTerm lxy_Pxy
\end{code}

\section{Parsing}

\begin{code}
variable = do
  c <- lower
  cs <- many (alphaNum <|> char '_')
  return $ Var (0, c:cs)

constant = do
  c <- upper
  cs <- many (alphaNum <|> char '_')
  return $ Const (0, c:cs)

varOrApply = do
  a <- variable
  option (VarAtom a) $ try $ do
    spaces
    char '('
    spaces
    ts <- term `sepBy` (spaces >> char ',' >> spaces)
    spaces
    char ')'
    return $ ApplyVar a ts

constOrApply = do
  a <- constant
  option (ConstAtom a) $ try $ do
    spaces
    char '('
    spaces
    ts <- term `sepBy` (spaces >> char ',' >> spaces)
    spaces
    char ')'
    return $ ApplyConst a ts

lambda = do
  char '\\'
  spaces
  vs <- variable `sepEndBy` spaces
  char '.'
  spaces
  t <- term
  return $ Lambda vs t
  
term = varOrApply <|> constOrApply <|> lambda

-- Test --

parseCheck s = case parse term "" s of
  Left  _ -> Nothing
  Right v -> Just v

prop_parseTerm_success = parseCheck "\\x y . P(x,y)" == Just lxy_Pxy
prop_parseTerm_fail = parseCheck "\\x y . P x,y)" == Nothing
\end{code}

\clearpage
\phantomsection
\addcontentsline{toc}{chapter}{Bibliography}
\bibliographystyle{plainnat}
\bibliography{AGI-book}

\end{document}