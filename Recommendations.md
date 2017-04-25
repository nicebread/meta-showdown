- Always assume heterogeneity (even in direct replications when multiple labs are involved --> cite ML1, McShane)
	- Do not trust p-curve and p-uniform, unless you have have convincing evidence that there is no heterogeneity (and how could you prove that?)
- Never trust a naive REMA, unless you have have convincing evidence that there is no publication bias and only medium amount of QRPs
- 3PSM works best if you have k>=30 studies


Rules:
- If a bias-correction method gives a significant result in the opposite direction, do not reject H0
	- Set negative bias-corrected estimates to zero
	- Ignore the CI
- Do not run p-curve and p-uniform on less than 4 significant studies
- Do not trust 3PSM (in this implementation) when it does not provide a p-value. Do not set the estimate to zero, treat it as "missing value".
- If p-uniform does not provide one of the CI boundaries, ignore the result. Do not set the estimate to zero, treat it as "missing value".