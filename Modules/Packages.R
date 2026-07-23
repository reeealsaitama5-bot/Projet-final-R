\section*{8.1 Packages R supplémentaires pour ce projet}

\subsection*{Packages pour la sécurité alimentaire}
\begin{itemize}
\item \texttt{RM.weights} : calcul du score FIES (Food Insecurity Experience Scale) avec pondération par les poids d'enquête. Fonction utilisée : \texttt{weighted.mean()} pour les moyennes pondérées.
    \item \texttt{FCIcalc} : calcul des scores de consommation alimentaire (HDDS, FCS) à partir des données de consommation (S07B). Permet d'agréger les groupes alimentaires selon la classification FAO.
\end{itemize}

\subsection*{Packages économétriques}
\begin{itemize}
\item \texttt{fixest} : estimation de modèles avec effets fixes de grappe (cluster robust standard errors). Alternative à \texttt{lm()} pour les régressions OLS avec effets fixes.
\item \texttt{estimatr} : calcul d'erreurs robustes pour les modèles linéaires (\texttt{lm\_robust()}), utile pour corriger l'hétéroscédasticité dans les régressions OLS.
\item \texttt{modelsummary} : génération de tableaux de résultats économétriques au format LaTeX, HTML ou Word. Permet d'exporter les sorties des modèles OLS, IV, logit, etc.
    \item \texttt{AER} : implémentation des modèles par variables instrumentales (IV/2SLS) avec la fonction \texttt{ivreg()}.
\end{itemize}

\subsection*{Packages pour l'analyse spatiale}
\begin{itemize}
\item \texttt{sf} : manipulation et visualisation de données vectorielles (shapefiles) pour les cartes des rendements et des marges.
\item \texttt{terra} : gestion de données raster pour l'intégration de données pluviométriques (CHIRPS) dans les cartes.
    \item \texttt{tmap} : création de cartes thématiques interactives ou statiques pour la visualisation des prix, rendements et marges par grappe.
    \item \texttt{leaflet} : cartes interactives pour le dashboard Shiny, permettant de visualiser les prix producteurs et les marges par grappe.
    \item \texttt{gstat} : interpolation spatiale (krigeage) des prix et rendements pour lisser les gradients de prix entre grappes.
\end{itemize}

\subsection*{Packages pour les données auxiliaires}
\begin{itemize}
    \item \texttt{WDI} : extraction des indicateurs de la Banque Mondiale (PIB, inflation, prix des denrées, etc.) pour contextualiser l'étude.
\item \texttt{FAOSTAT} : téléchargement des données agricoles de la FAO (superficies, rendements, bilans alimentaires).
\item \texttt{geodata} : téléchargement de fonds de carte GADM pour les pays de l'UEMOA.
    \item \texttt{chirps} : téléchargement des données pluviométriques CHIRPS pour la superposition sur les cartes de rendements.
    \item \texttt{osmdata} : récupération des données OpenStreetMap (distances, infrastructures, marchés) pour l'analyse de l'accès aux marchés.
\end{itemize}

\subsection*{Packages de visualisation}
\begin{itemize}
    \item \texttt{ggplot2} : création de graphiques statiques pour le rapport (histogrammes des rendements, diagrammes en barres des marges, etc.).
    \item \texttt{plotly} : graphiques interactifs pour le dashboard Shiny (distribution du FIES, comparaison des modèles, etc.).
    \item \texttt{treemap} : visualisation de la composition des revenus agricoles par culture sous forme de treemap.
    \item \texttt{viridis} : palettes de couleurs pour les cartes et les graphiques, assurant une meilleure lisibilité pour les déficiences visuelles.
\end{itemize}

\subsection*{Tableau récapitulatif des packages}

\begin{table}[H]
\centering
\rowcolors{2}{lightblue}{white}
\renewcommand{\arraystretch}{1.3}
\resizebox{\textwidth}{!}{
\begin{tabular}{p{0.2\textwidth} p{0.35\textwidth} p{0.35\textwidth}}
\toprule
\rowcolor{mainblue} \tblhead{Catégorie} & \tblhead{Packages} & \tblhead{Utilisation dans le projet} \\
\midrule
Sécurité alimentaire & \texttt{RM.weights}, \texttt{FCIcalc} & Calcul des scores FIES, HDDS, FCS avec pondération. \\
Économétrie & \texttt{fixest}, \texttt{estimatr}, \texttt{modelsummary}, \texttt{AER} & Régression OLS, IV/2SLS, erreurs robustes, tableaux de résultats. \\
Spatial & \texttt{sf}, \texttt{terra}, \texttt{tmap}, \texttt{leaflet}, \texttt{gstat} & Cartes des rendements, prix, marges, interpolation spatiale. \\
Données auxiliaires & \texttt{WDI}, \texttt{FAOSTAT}, \texttt{geodata}, \texttt{chirps}, \texttt{osmdata} & Données macro, pluviométrie, fonds de carte, infrastructures. \\
Visualisation & \texttt{ggplot2}, \texttt{plotly}, \texttt{treemap}, \texttt{viridis} & Graphiques statiques et interactifs, palette de couleurs. \\
\bottomrule
\end{tabular}
}
\caption{Packages R spécifiques au projet de filière}
\label{tab:packages}
\end{table}