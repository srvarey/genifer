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

\definecolor{DarkGreen}{rgb}{0.1,0.4,0.1}

\newcommand{\formula}[1]{\textcolor{DarkGreen}{#1}}
\renewcommand{\star}{$* \;$}
\newcommand{\app}{$\cdot \;$}
\newcommand{\eg}{\textbf{Example:} }
\newcommand{\tab}{\hspace*{1cm} }
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
\author{YKY (general.intelligence@@Gmail.com)}
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

\section{CloudHaskell}

\begin{code}
import Remote

import Data.Typeable (Typeable)
import Data.Data (Data)
import Data.Binary (Binary,get,put)
import System.Random (randomR,getStdRandom)
import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Control.Monad.Trans (liftIO)

main = putStrLn "Hello, World!"
\end{code}

\chapter{TV evaluation}
\label{ch:tv-evaluation}

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

\chapter{First-order logic}

\section{Unification}

The following code is borrowed from Takashi Yamamiya's web site:\\
\tab http://propella.blogspot.com/2009/04/prolog-in-haskell.html

The type:
\begin{spec}
type Substitution = [(Term, Term)]
true = []
\end{spec}

This is the code for applying a substitution:
\begin{spec}
-- apply [(w"X", w"Y"), (w"Y", w"Z")] [(w"X"), (w"Y")] == [(w"Z"), (w"Z")]
apply :: Substitution -> [Term] -> [Term]
apply s ts = [applyTerm s t | t <- ts]

applyTerm [] (Var y n)                                  = Var y n
applyTerm ((Var x i, t):s) (Var y j) | x == y && i == j = applyTerm s t
                                     | otherwise        = applyTerm s (Var y j)
applyTerm s (Struct n ts)                               = Struct n (apply s ts)
\end{spec}

And here is unify:
\begin{spec}
-- unify (w"X") (w"apple") == Just [(w"X", w"apple")]
unify :: Term -> Term -> Maybe Substitution
unify (Var x n) (Var y m) = Just [(Var x n, Var y m)]
unify (Var x n)      y    = Just [(Var x n,       y)]
unify      x    (Var y m) = Just [(Var y m,       x)]
unify (Struct a xs) (Struct b ys)
      | a == b = unifyList xs ys
      | otherwise   = Nothing

unifyList :: [Term] -> [Term] -> Maybe Substitution
unifyList [] [] = Just true
unifyList [] _ = Nothing
unifyList _ [] = Nothing
unifyList (x:xs) (y:ys) = do s <- unify x y
                             s' <- unifyList (apply s xs) (apply s ys)
                             return (s ++ s')
\end{spec}

\section{Substitution management}

%\chapter{Memory}

%\chapter{Induction}

%\chapter{Main}

\end{document}