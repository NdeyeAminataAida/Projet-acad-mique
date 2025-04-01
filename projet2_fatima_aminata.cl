;;; Base des faits initiale
(defparameter *BdF*
  '((glycemie 0.6)
    (pain-complet) (flocon-mais) (son-avoine) (orge) (riz-etuve)
    (haricots-rouges) (pois-chiches) (riz-basmati) (gruau) (couscous)
    (pain-blanc) (riz-souffle) (haricots-verts) (brocolis) (epinards)
    (pomme-crue) (poire) (2-pommes-de-terre) (2-tranches-pain-complet) (yaourt)
    (fromage-30g) (tasse-lait) (dattes-fraiches) (2-morceaux-sucre)
    (insuline-rapide) (insuline-prandiale) 
    (legume) (1-portion-poisson) (2-portions-poisson-semaine)
    (quantite-glucides) (quantite-legume) (quantite-fruits) (quantite-pates)))

;;; Base des règles
(defparameter *BdR*
  '(
    (R1 (< glycemie 0.7) (hypoglycemie))
    (R2 (hypoglycemie) (or (> repas-IG 70) (15g-glucides)))
    (R3 (15g-glucides) (or (dattes-fraiches) (2-morceaux-sucre)))
    (R4 (hypoglycemie-corrigee) (glycemie-normale))
    (R5 (glycemie-normale) (repas-equilibre))
   )
  )

;;; Fonction pour ajouter un fait dans la base des faits
(defun ajouter-fait (fait)
  "Ajoute un fait à la base des faits s'il n'existe pas déjà."
  (unless (member fait *BdF* :test #'equal)
    (push fait *BdF*)))

;;; Fonction pour extraire la prémisse d'une règle
(defun premisse (regle)
  "Retourne la prémisse de la règle."
  (cadr regle))

;;; Fonction pour extraire la conclusion d'une règle
(defun conclusion (regle)
  "Retourne la conclusion de la règle."
  (caddr regle))

;;; Fonction pour évaluer une condition en fonction de la base des faits
(defun evaluer-condition (condition)
  "Évalue une condition simple ou une comparaison numérique avec la base des faits."
  (cond
    ;; Si la condition est une comparaison numérique
    ((and (listp condition) (member (car condition) '(< > <= >= =)))
     ;; Remplace la variable `glycemie` par sa valeur dans la base des faits
     (let ((glycemie (cadr (assoc 'glycemie *BdF* :test #'equal)))) ; Trouver `glycemie` dans *BdF*
       (if glycemie
           (eval `(let ((glycemie ,glycemie)) ,condition)) ; Évalue avec la bonne valeur
           nil))) ; Retourne nil si `glycemie` n'est pas définie
    ;; Si la condition est un fait simple
    ((member condition *BdF* :test #'equal) t)
    ;; Sinon, retourne nil
    (t nil)))

;;; Fonction principale de chaînage avant
(defun chainage-avant (*BdF* *BdR*)
  "Effectue un chaînage avant en parcourant les règles."
  (let ((resultats '())) ; Liste pour stocker les règles satisfaites
    (dolist (regle *BdR*) ; Parcourt chaque règle de la base des règles
      (let* ((premisse-actuelle (premisse regle))
             (conclusion-actuelle (conclusion regle)))
        ;; Vérifier si la prémisse est satisfaite
        (when (evaluer-condition premisse-actuelle)
          ;; Ajouter la règle aux résultats
          (push conclusion-actuelle resultats)
          ;; Ajouter la conclusion à la base des faits
          (ajouter-fait conclusion-actuelle)
          ;; Traiter les conclusions complexes (or, and)
          (when (or (equal (car conclusion-actuelle) 'or)
                    (equal (car conclusion-actuelle) 'and))
            (dolist (sous-fait (cdr conclusion-actuelle))
              (unless (member sous-fait *BdF* :test #'equal)
                (ajouter-fait sous-fait)))))))
    ;; Afficher les résultats finaux
    (format t "Base des faits finale : ~a~%" *BdF*)
    (format t "Explication: a~%")
    (format t "Voici quelques recommandations nutritionnelles : ~a~%" resultats)))



 
