(ns integrador.core
  (:gen-class)
  (:require [clojure.string :as str]))

(defn cadastrar-alunos []
  (println "\n=== Cadastro de Alunos ===")
  (loop [alunos []]
    (print "Nome do aluno (ou ENTER para encerrar): ")
    (flush)
    (let [nome (read-line)]
      (if (str/blank? nome)
        alunos
        (do
          (print "Nota do aluno: ")
          (flush)
          (let [nota (Double/parseDouble (read-line))
                aluno {:nome nome :nota nota}]
            (recur (conj alunos aluno))))))))

(defn calcular-status [aluno]
  (assoc aluno :status (if (>= (:nota aluno) 6.0) "Aprovado" "Reprovado")))

(defn relatorio-notas [alunos]
  (if (empty? alunos)
    (do
      (println "\nNenhum aluno cadastrado.")
      alunos)
    (let [alunos-status (map calcular-status alunos)
          aprovados (filter #(= (:status %) "Aprovado") alunos-status)
          media (/ (reduce + (map :nota alunos)) (count alunos))]
      (println "\n=== Relatorio de Notas ===")
      (println "\n--- Todos os alunos ---")
      (doseq [a alunos-status]
        (println (:nome a) "- Nota:" (:nota a) "- Status:" (:status a)))
      (println "\n--- Aprovados ---")
      (doseq [a aprovados]
        (println (:nome a) "- Nota:" (:nota a)))
      (println (format "\nMedia geral da turma: %.2f" media))
      alunos)))

(defn estatisticas [alunos]
  (if (empty? alunos)
    (do
      (println "\nNenhum aluno cadastrado.")
      alunos)
    (let [alunos-status (map calcular-status alunos)
          total (count alunos)
          aprovados (count (filter #(= (:status %) "Aprovado") alunos-status))
          reprovados (- total aprovados)
          notas (map :nota alunos)
          maior (apply max notas)
          menor (apply min notas)
          media (/ (reduce + notas) total)]
      (println "\n=== Estatísticas Gerais ===")
      (println "Total de alunos:" total)
      (println "Aprovados:" aprovados)
      (println "Reprovados:" reprovados)
      (println "Maior nota:" maior)
      (println "Menor nota:" menor)
      (println (format "Media geral: %.2f" media))
      alunos)))

(defn buscar-aluno [alunos]
  (if (empty? alunos)
    (do
      (println "\nNenhum aluno cadastrado.")
      alunos)
    (do
      (print "\nDigite o nome do aluno que deseja buscar: ")
      (flush)
      (let [nome (read-line)
            resultado (first
                        (filter #(= (str/lower-case (:nome %))
                                    (str/lower-case nome))
                                (map calcular-status alunos)))]
        (if resultado
          (println (:nome resultado) "- Nota:" (:nota resultado) "- Status:" (:status resultado))
          (println "Aluno nao encontrado.")))
      alunos)))

(defn menu []
  (loop [alunos []]
    (println "\n=== MENU PRINCIPAL ===")
    (println "1 - Cadastrar Alunos")
    (println "2 - Relatorio de Notas")
    (println "3 - Estatisticas Gerais")
    (println "4 - Buscar Aluno pelo Nome")
    (println "0 - Sair")
    (print "Escolha uma opcao: ")
    (flush)
    (let [opcao (read-line)]
      (cond
        (= opcao "1") (recur (cadastrar-alunos))
        (= opcao "2") (recur (relatorio-notas alunos))
        (= opcao "3") (recur (estatisticas alunos))
        (= opcao "4") (recur (buscar-aluno alunos))
        (= opcao "0") (println "\nSaindo do sistema...")
        :else (do
                (println "Opção invalida!")
                (recur alunos))))))

(defn -main [& _]
  (menu))
