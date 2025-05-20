import sys
import threading
import multiprocessing
import pandas as pd
import difflib
from PyQt5 import uic
from PyQt5.QtWidgets import QApplication, QMessageBox

# -------------------------------
# Carregamento dos dados (CORRIGIDO)
# -------------------------------
municipios = pd.read_csv("municipios.csv")
estados = pd.read_csv("estados.csv")

# Merge usando a sigla do estado (uf/estado)
municipios = pd.merge(
    municipios,
    estados[["uf", "regiao"]],  # Seleciona apenas as colunas necessárias
    left_on="estado",          # Coluna em municipios.csv
    right_on="uf",              # Coluna em estados.csv
    how="left"
)

# -------------------------------
# Padrão Prototype: Thread de busca clonável
# -------------------------------
class BuscaThread(threading.Thread):
    def __init__(self, estado, nome_cidade, resultado):
        super().__init__()
        self.estado = estado
        self.nome_cidade = nome_cidade.lower()
        self.resultado = resultado

    def run(self):
        subset = municipios[
            (municipios["uf"] == self.estado) & 
            (municipios["nome"].str.lower() == self.nome_cidade)
        ]
        for _, cidade in subset.iterrows():
            self.resultado.append(cidade.to_dict())

    def clone(self, novo_estado):
        return BuscaThread(novo_estado, self.nome_cidade, self.resultado)

# -------------------------------
# Padrão Factory: Fábrica de Threads
# -------------------------------
class ThreadFactory:
    def __init__(self, prototype, estados):
        self.prototype = prototype
        self.estados = estados

    def criar_threads(self):
        return [self.prototype.clone(estado) for estado in self.estados]

# -------------------------------
# Padrão Factory: Fábrica de Processos
# -------------------------------
class ProcessFactory:
    def __init__(self, regioes, nome_cidade, resultado):
        self.regioes = regioes
        self.nome_cidade = nome_cidade
        self.resultado = resultado

    def criar_processos(self):
        processos = []
        for regiao in self.regioes:
            p = multiprocessing.Process(
                target=self._processo_regional,
                args=(regiao, self.nome_cidade, self.resultado)
            )
            processos.append(p)
        return processos

    def _processo_regional(self, regiao, nome_cidade, resultado):
        estados_da_regiao = municipios[municipios["regiao"] == regiao]["uf"].unique()
        prototype = BuscaThread(None, nome_cidade, resultado)
        factory = ThreadFactory(prototype, estados_da_regiao)
        threads = factory.criar_threads()

        for thread in threads:
            thread.start()
        for thread in threads:
            thread.join()

# -------------------------------
# Interface Gráfica com PyQt5
# -------------------------------
class MainApp:
    def __init__(self, path_ui='interface.ui'):
        Form, Window = uic.loadUiType(path_ui)
        self.app = QApplication([])
        self.window = Window()
        self.form = Form()
        self.form.setupUi(self.window)

        self.form.lineEdit_estado.hide()
        self.form.label_sugestao.setText("")
        self.form.pushButton.clicked.connect(self.busca_concorrente)
        self.form.lineEdit_estado.returnPressed.connect(self.busca_com_estado)

    def busca_concorrente(self):
        nome_cidade = self.form.lineEdit.text().strip()
        self.form.label_sugestao.setText("")
        self.form.lineEdit_estado.hide()

        if not nome_cidade:
            self.form.label.setText("Digite o nome da cidade.")
            return

        manager = multiprocessing.Manager()
        resultado = manager.list()

        regioes = municipios["regiao"].unique()
        factory = ProcessFactory(regioes, nome_cidade, resultado)
        processos = factory.criar_processos()

        for p in processos:
            p.start()
        for p in processos:
            p.join()

        if len(resultado) == 0:
            sugestoes = self.obter_sugestoes(nome_cidade)
            if sugestoes:
                self.form.label_sugestao.setText(f"Sugestões: {', '.join(sugestoes)}")
            else:
                self.form.label.setText("Cidade não encontrada.")
        elif len(resultado) == 1:
            self.exibir_resultado(resultado[0])
        else:
            estados = list({c['uf'] for c in resultado})
            self.form.label_sugestao.setText(
                f"Multiplas cidades encontradas. Selecione o estado: {', '.join(estados)}")
            self.form.lineEdit_estado.setHidden(False)
            self.form.label.setText("Insira a sigla do estado:")

    def busca_com_estado(self):
        estado = self.form.lineEdit_estado.text().strip().upper()
        nome_cidade = self.form.lineEdit.text().strip()
        
        cidades = municipios[
            (municipios["nome"].str.lower() == nome_cidade.lower()) & 
            (municipios["uf"] == estado)
        ]
        
        if len(cidades) == 0:
            QMessageBox.warning(self.window, "Erro", "Estado inválido para esta cidade.")
        else:
            self.exibir_resultado(cidades.iloc[0].to_dict())

    def exibir_resultado(self, cidade):
        texto = (f"{cidade['nome']} - {cidade['uf']} ({cidade['regiao']})\n"
                f"Latitude: {cidade['latitude']}, Longitude: {cidade['longitude']}")
        self.form.label.setText(texto)
        self.form.lineEdit_estado.hide()

    def obter_sugestoes(self, nome_cidade):
        todas_cidades = municipios['nome'].str.lower().unique()
        sugestoes = difflib.get_close_matches(
            nome_cidade.lower(), todas_cidades, n=3, cutoff=0.6
        )
        return [s.capitalize() for s in sugestoes]

    def show(self):
        self.window.show()

# -------------------------------
# Execução principal
# -------------------------------
if __name__ == "__main__":
    app = QApplication(sys.argv)
    main = MainApp()
    main.show()
    sys.exit(app.exec_())