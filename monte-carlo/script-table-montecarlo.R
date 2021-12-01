setwd(here::here("monte-carlo"))
mc_beta <- readRDS("gamma_fixed.Rds")
mc_beta_bis <- readRDS("gamma_fixed_diff.Rds")
mc_beta_ter <- readRDS("gamma_fixed_true_diff.Rds")
mc_gamma <- readRDS("beta_fixed.Rds")
mc_gamma_diff <- readRDS("beta_fixed_true_diff.Rds")


x1 <- lapply(1:length(mc_beta), function(i) mc_beta[[i]]$estimates$par)
x2 <- lapply(1:length(mc_beta_bis), function(i) mc_beta_bis[[i]]$estimates$par)
x3 <- lapply(1:length(mc_beta_ter), function(i) mc_beta_ter[[i]]$estimates$par)
x4 <- lapply(1:length(mc_gamma), function(i) mc_gamma[[i]]$estimates$par)
x5 <- lapply(1:length(mc_gamma_diff), function(i) mc_gamma_diff[[i]]$estimates$par)


t <- data.frame(as.numeric(x1), as.numeric(x3), as.numeric(x2),
                as.numeric(x4), as.numeric(x5))

rnt <- c("$\\quad$ Main specification",
                 "$\\quad$ Restrict to population aged from 40 to 60",
                 "$\\quad$ Main specification",
                 "$\\quad$ Restrict to population aged from 40 to 60",
                 "$\\quad$ Main specification",
                 "$\\quad$ Restrict to population aged from 40 to 60")

bod <- lapply(1:nrow(t), function(i) {
  xx <- paste(format(t[i,], digits = 2, nsmall = 3), collapse = " & ")
  sprintf("%s & %s \\\\", rnt[i], xx)
}) 
bod <- as.character(bod)
bod <- c("\\textit{All moments:}  &  \\\\",
         bod[1:2],
         "\\textit{First set of moment only:}  &  \\\\",
         bod[3:4],
         "\\textit{Second set of moment only:}  &  \\\\",
         bod[5:6]
)
bod <- paste(bod, collapse = '\n')


header <- "
\\begin{table}[ht]
\\centering
\\caption{Convergence of estimations on a setting where the true $(r, \\beta, \\gamma)$ are known} 
\\label{tab: monte-carlo-beta}
\\begin{tabular}{p{0.35\\linewidth}ccccc}
\\toprule
& \\multicolumn{3}{c}{Experience on $\\beta$} & \\multicolumn{2}{c}{Experience on $\\gamma$} \\\\
\\textit{Scenario:} & \\multicolumn{2}{c}{(1)} & \\multicolumn{1}{c}{(2)} & \\multicolumn{1}{c}{(1)} & \\multicolumn{1}{c}{(3)} \\\\ 
 & $\\beta^{(0)}=0.75$ & $\\beta^{(0)}=1.2$ & $\\beta^{(0)}=0.75$ & $\\gamma^{(0)}=1.2$ & $\\gamma^{(0)}=3$ \\\\
\\midrule
"

bot <- "
\\midrule
\\bottomrule
\\textit{Scenario:}  &  \\\\ 
\\multicolumn{4}{l}{$\\quad$ (1): $(r=3\\%, \\beta=0.98, \\gamma=2)$} \\\\
\\multicolumn{4}{l}{$\\quad$ (2): $(r=8\\%, \\beta=0.96, \\gamma=2)$} \\\\
\\multicolumn{4}{l}{$\\quad$ (3): $(r=3\\%, \\beta=0.98, \\gamma=0.5)$}
\\end{tabular}
\\end{table}
"

tab <- sprintf("%s %s \n %s", header, bod, bot)

cat(tab)

xtable::xtable(t, type = "latex",
               caption = "Convergence of estimations on a setting where the true $(r, \\beta, \\gamma)$ are known",
               digits = 5)


