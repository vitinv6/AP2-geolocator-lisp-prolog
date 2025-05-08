(defun gerar-dados (n)
  (loop for i from 0 below n
        collect (list (+ 2001 i) (random 11))))  ; RA e nota entre 0 e 10

(defun filtrar-alunos (alunos)
  "Retorna lista com apenas alunos com nota >= 7."
  (remove-if-not (lambda (aluno) (>= (second aluno) 7)) alunos))

(defun calcular-media (notas)
  "Calcula a média de uma lista de números."
  (if notas
      (/ (reduce #'+ notas) (length notas))
      0))

(defun extrair-notas (alunos)
  "Extrai apenas as notas de uma lista de alunos."
  (mapcar #'second alunos))

;; Execução principal
(let* ((alunos (gerar-dados 10))) ; use 10 para facilitar a visualização
  (format t "Lista de alunos gerada:~%")
  (dolist (aluno alunos)
    (format t "  RA: ~a, Nota: ~a~%" (first aluno) (second aluno)))

  (let* ((alunos-filtrados (filtrar-alunos alunos)))
    (format t "~%Alunos com nota >= 7:~%")
    (dolist (aluno alunos-filtrados)
      (format t "  RA: ~a, Nota: ~a~%" (first aluno) (second aluno)))

    (let* ((notas-filtradas (extrair-notas alunos-filtrados)))
      (format t "~%Notas filtradas: ~a~%" notas-filtradas)

      (let ((media (calcular-media notas-filtradas)))
        (format t "~%Média das notas filtradas: ~,2f~%" media)))))
