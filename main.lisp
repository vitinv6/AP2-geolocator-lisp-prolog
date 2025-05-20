(defun split-by-comma (linha)
  "Divide uma string por vírgulas."
  (let ((result '())
        (start 0)
        (len (length linha)))
    (loop for i from 0 below len
          do (if (char= (char linha i) #\,)
                 (progn
                   (push (subseq linha start i) result)
                   (setf start (1+ i)))))
    (push (subseq linha start) result)
    (nreverse result)))

(defun ler-arquivo-notas (caminho)
  "Lê o arquivo de notas e retorna uma lista de listas (RA Nota)."
  (with-open-file (in caminho :direction :input)
    ;; Pula a primeira linha (cabeçalho)
    (read-line in nil nil)
    (loop for linha = (read-line in nil nil)
          while linha
          collect (let* ((partes (split-by-comma linha))
                         (ra-str (string-trim " " (first partes)))
                         (nota-str (string-trim " " (second partes))))
                    (list (parse-integer ra-str)
                          (parse-integer nota-str))))))

(defun filtrar-alunos (alunos)
  "Retorna lista com apenas alunos com nota >= 7."
  (remove-if-not (lambda (aluno) (>= (second aluno) 7)) alunos))

(defun calcular-media (notas)
  "Calcula a média de uma lista de números."
  (if notas
      (/ (reduce #'+ notas) (length notas) 1.0)
      0.0))

(defun extrair-notas (alunos)
  "Extrai apenas as notas de uma lista de alunos."
  (mapcar #'second alunos))

;; Execução principal
(let* ((alunos (ler-arquivo-notas "notas.txt")))
  (let* ((alunos-filtrados (filtrar-alunos alunos))
         (primeiros-10 (subseq alunos-filtrados 0 (min 10 (length alunos-filtrados)))))
    (format t "~%Primeiros 10 alunos com nota >= 7:~%")
    (dolist (aluno primeiros-10)
      (format t "  RA: ~a, Nota: ~a~%" (first aluno) (second aluno)))

    (let* ((notas-escolhidas (extrair-notas primeiros-10)))
      (format t "~%Notas escolhidas (até 10): ~a~%" notas-escolhidas)

      (let ((media (calcular-media notas-escolhidas)))
        (format t "~%Média dessas notas: ~,2f~%" media)))))