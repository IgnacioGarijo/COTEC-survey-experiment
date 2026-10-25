"""One-off approved editorial changes from 8c07f4c; never run during routine builds."""
from pathlib import Path
import subprocess,re
R=Path(__file__).resolve().parents[2];D=R/'4. Latex/paper_A'
def baseline(name):return subprocess.check_output(['git','show','8c07f4c:4. Latex/paper_A/'+name],cwd=R).decode('utf8')
def uncolor(s):
 while (m:=re.search(r'\\(?:old|rev|new)\{',s)):
  a=m.end();k=a;depth=1
  while depth:
   if s[k]=='{' and s[k-1]!='\\':depth+=1
   if s[k]=='}' and s[k-1]!='\\':depth-=1
   k+=1
  s=s[:m.start()]+s[a:k-1]+s[k:]
 s=re.sub(r'\\(?:begin|end)\{(?:revision|addition)\}\n?','',s)
 return s.replace('{revised}','{reused}').replace('{added}','{reused}')
s=uncolor(baseline('paper_A_teacher_decisions.tex'))
def para(prefix,value):
 global s
 a=s.index(prefix);b=s.index('\n\n',a);s=s[:a]+value+s[b:]
# Replace introduction opening only, leaving author abstract unchanged.
a=s.index('\\section{Introduction}');b=s.index('\n\nThe distinction matters',a)
s=s[:a]+r'''\section{Introduction}
Grade retention changes a student's educational trajectory and the peer group in which schooling continues. Its consequences are not uniform: reviews document variation across settings, research designs and comparison groups \citep{allen,goos,valbuena}. Social relationships may also be affected. In Spain, a matching-based study associates retention with fewer friendship nominations and greater involvement in hostile peer networks \citep{branasjorrat}. These academic and social stakes make it important to understand not only what repetition accomplishes, but how teachers decide whom to recommend for it.'''+s[b:]
para('The educational case for repetition is contingent.',r'''Because the educational case for repetition is contingent, academic difficulties do not uniquely determine whether another year in the same grade is an appropriate response. Our framework distinguishes teachers' assessment of difficulties if the student progresses from their assessment of the gains and costs of making the student repeat. Studies of recommendations and studies of the consequences of actual retention therefore address complementary questions.''')
para("Research on teachers' beliefs provides",r'''Teachers' judgments can incorporate information beyond demonstrated academic performance. Comparing teacher-assigned and blindly assigned scores in Brazil, \citet{ferman} find that classroom behavior enters grading and promotion decisions. In a British experiment, \citet{doyle} find that an identical piece of written work receives less favorable evaluations when attributed to a student of lower socioeconomic status. These studies motivate separating academic signals, conduct and social circumstances, while recognizing that grading a piece of work and recommending grade retention are different judgments.

Research on retention beliefs provides a complementary foundation. Tomchin and Impara \citeyearpar{tomchin} examine the reasoning underlying retention judgments, and \citet{bonvin} connects attitudes and judgments to retention decisions. Beliefs are not a single construct: empathy, views about the role of effort and assessments of educational resources capture different considerations \citep{pajares}. We use these measures to characterize observed harshness and to distinguish measured teacher information from patterns revealed by decisions.''')
para('We compare three prespecified-for-this-revision',r'''The diagonal specification is the stable interior solution among three covariance structures compared on the same data. We retain correlated-pair and unrestricted specifications as sensitivities in Appendix~\ref{app:prediction}. Zero covariances are a working restriction, not a finding that teachers' responses to different attributes are unrelated.''')
# Remove main numerical exhibits; preserve full tables in existing appendix B.
for name in ['attributes','interactions','academic']:
 s=re.sub(r'\\inputtab\{[^}]+\}\{'+name+r'\}\{[^\n]*\}\{[^}]+\}\n?','',s)
s=s.replace('The within-teacher coefficients in Table~\\ref{tab:attr}', 'The within-teacher coefficients in Figure~\\ref{fig:attributes}')
anchor='Attendance and conduct retain'
pos=s.index(anchor)
s=s[:pos]+r'''\plot{reused}{attribute_coefficients}{Within-teacher responses to the six student attributes. Points are percentage-point coefficients; horizontal bars show one teacher-clustered standard error. The vertical line marks zero. Numerical estimates, SE and significance stars are reported in Table~\ref{tab:attr}.}{fig:attributes}

'''+s[pos:]
para('The interaction specification in',r'''The interaction specification in Table~\ref{tab:interactions} shows that failed subjects and low competences reinforce one another. Low competences add 21.45 percentage points when the failed-subject indicator is absent and 32.52 when it is present, a difference of 11.07 points that survives Holm adjustment across the three interaction tests. The interaction of failures with discipline is smaller (2.18 points) but also survives this adjustment; the attendance interaction is imprecise. Thus combining academic difficulties produces the strongest reinforcement, alongside a more modest additional role for discipline when subjects are failed. The next section examines cases in which academic signals point in different directions.''')
s=s.replace('Table~\\ref{tab:academic} locates','Figure~\\ref{fig:academic} locates')
anchor='Tables~\\ref{tab:tercsummary}'
pos=s.index(anchor)
s=s[:pos]+r'''\plot{reused}{academic_profiles}{Observed repetition recommendations and same-profile disagreement across four combinations of failed subjects and competences. Shaded rows indicate conflicting academic signals. The left panel shows category rates with one teacher-clustered SE; the right shows frequency-weighted disagreement calculated within six-attribute profiles, without uncertainty bars. Remaining attributes are not standardized across categories. Table~\ref{tab:academic} gives the numerical estimates.}{fig:academic}

Within a profile, pairwise disagreement is greatest when recommendations divide approximately equally. Across a category containing several profiles, however, a repetition rate close to one half can also reflect consensus in opposite directions on different profiles. Computing disagreement within each six-attribute profile distinguishes these possibilities. The high disagreement for failures with adequate competences therefore describes division on comparable cases, rather than only an aggregate balance between repetition and progression.

'''+s[pos:]
para('Table~\\ref{tab:char} compares',r'''Table~\ref{tab:char} compares models of harshness on the same 2,164 teachers. Professional and demographic characteristics account for 2.58 percent of its observed variation; adding general beliefs raises this to 2.88 percent, and adding agreement about resources raises it to 4.68 percent. Secondary teachers are somewhat more severe on average, and older teachers less severe when compared at the same level of experience. Experience itself has a small, uncertain coefficient. These associations coexist with substantial overlap between groups and limited overall fit.
\inputtab{reused}{characterization}{Observed characteristics associated with harshness}{tab:char}''')
para('Attendance, discipline and background extend',r'''Attendance, discipline and background extend this account beyond grades. The role of conduct is consistent with evidence that behavior enters teachers' grading and promotion judgments \citep{ferman}, although our coefficients do not identify whether teachers expect educational gains or apply normative criteria. The negative complex-background coefficient is compatible with teachers taking the costs of repetition into account. It need not conflict with less favorable grading of identical work attributed to low-SES students \citep{doyle}: the stated background and the decision being evaluated differ. Likewise, evidence connecting gender differences in grading to subsequent achievement \citep{terrier} can coexist with our small average gender coefficient in retention recommendations conditional on the information shown. These comparisons distinguish judgments on comparable profiles from the full process generating population disparities.''')
s=s.replace('\\section{Alternative definitions of harshness}', '\\section{Supporting estimates and alternative definitions of harshness}')
anchor='\\section{Supporting estimates and alternative definitions of harshness}\\label{app:harshness}'
s=s.replace(anchor,anchor+r'''
Tables~\ref{tab:attr}--\ref{tab:academic} provide the numerical estimates underlying the main coefficient and academic-profile figures and the interaction discussion. Significance stars and standard errors follow the conventions defined in the main text.
\inputtab{reused}{attributes}{Within-teacher responses to student attributes}{tab:attr}
\inputtab{reused}{interactions}{Combining academic and behavioral information within teachers}{tab:interactions}
\inputtab{reused}{academic}{Academic profiles and disagreement on the same six-attribute profile}{tab:academic}
\FloatBarrier
''')
s=s.replace('\\input{preamble}', '\\input{preamble_content}')
(D/'paper_A_content.tex').write_text(s,encoding='utf8')
p=baseline('preamble.tex')
p=re.sub(r'(\\definecolor\{(?:reused|revised|added)\}\{HTML\})\{[^}]+\}',r'\1{000000}',p)
(D/'preamble_content.tex').write_text(p,encoding='utf8')
refs=uncolor(baseline('references_A.tex'))
entries=[('Bonvin',r'''\bibitem[Bra\~nas-Garza \& Jorrat(2025)]{branasjorrat} Bra\~nas-Garza, P., \& Jorrat, D. (2025). La repetici\'on perjudica la inclusi\'on social en la escuela. \textit{Revista de Educaci\'on, 409}, 28--58. \url{https://doi.org/10.4438/1988-592X-RE-2025-409-685}'''),('Davoudzadeh',r'''\bibitem[Doyle et al.(2023)]{doyle} Doyle, L., Easterbrook, M. J., \& Harris, P. R. (2023). Roles of socioeconomic status, ethnicity and teacher beliefs in academic grading. \textit{British Journal of Educational Psychology, 93}(1), 91--112. \url{https://doi.org/10.1111/bjep.12541}
\bibitem[Ferman \& Fontes(2022)]{ferman} Ferman, B., \& Fontes, L. F. (2022). Assessing knowledge or classroom behavior? Evidence of teachers' grading bias. \textit{Journal of Public Economics, 216}, Article 104773. \url{https://doi.org/10.1016/j.jpubeco.2022.104773}'''),('Santos',r'''\bibitem[Terrier(2020)]{terrier} Terrier, C. (2020). Boys lag behind: How teachers' gender biases affect student achievement. \textit{Economics of Education Review, 77}, Article 101981. \url{https://doi.org/10.1016/j.econedurev.2020.101981}''')]
for after,entry in entries:
 lines=refs.splitlines();idx=next(i for i,l in enumerate(lines) if l.startswith('\\bibitem['+after));lines.insert(idx+1,entry);refs='\n'.join(lines)+'\n'
refs=refs.replace('\\end{thebibliography}',r'''\bibitem[Valbuena et al.(2021)]{valbuena} Valbuena, J., Mediavilla, M., Choi, \'A., \& Gil, M. (2021). Effects of grade retention policies: A literature review of empirical studies applying causal inference. \textit{Journal of Economic Surveys, 35}(2), 408--451. \url{https://doi.org/10.1111/joes.12406}
\end{thebibliography}''')
(D/'references_A.tex').write_text(refs,encoding='utf8')
