import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from scipy import stats
from typing import Dict, List

class AnalisisInnovacionDigital:
    def __init__(self, data: pd.DataFrame):
        """
        Inicializa el análisis con un DataFrame de pandas.
        
        Parameters:
        data (pd.DataFrame): DataFrame con los datos de la encuesta
        """
        self.data = data
        self.resultados = {}
        
    def estadistica_descriptiva(self) -> Dict:
        """
        Calcula estadísticas descriptivas básicas para variables numéricas.
        """
        stats_desc = self.data.describe()
        self.resultados['estadistica_descriptiva'] = stats_desc
        return stats_desc
    
    def analisis_frecuencias(self, columnas: List[str]) -> Dict[str, pd.Series]:
        """
        Calcula tablas de frecuencia para variables categóricas.
        
        Parameters:
        columnas (List[str]): Lista de nombres de columnas categóricas
        """
        frecuencias = {}
        for col in columnas:
            freq = self.data[col].value_counts()
            freq_rel = self.data[col].value_counts(normalize=True) * 100
            frecuencias[col] = pd.concat([freq, freq_rel], axis=1, 
                                       keys=['Frecuencia', 'Porcentaje'])
        self.resultados['frecuencias'] = frecuencias
        return frecuencias
    
    def visualizar_distribucion(self, columna: str, titulo: str):
        """
        Crea un histograma para variables numéricas.
        
        Parameters:
        columna (str): Nombre de la columna a visualizar
        titulo (str): Título del gráfico
        """
        plt.figure(figsize=(10, 6))
        sns.histplot(data=self.data, x=columna, kde=True)
        plt.title(titulo)
        plt.xlabel(columna)
        plt.ylabel('Frecuencia')
        plt.show()
        
    def grafico_barras(self, columna: str, titulo: str):
        """
        Crea un gráfico de barras para variables categóricas.
        
        Parameters:
        columna (str): Nombre de la columna a visualizar
        titulo (str): Título del gráfico
        """
        plt.figure(figsize=(10, 6))
        self.data[columna].value_counts().plot(kind='bar')
        plt.title(titulo)
        plt.xlabel(columna)
        plt.ylabel('Frecuencia')
        plt.xticks(rotation=45)
        plt.show()
        
    def analisis_correlacion(self, variables: List[str]):
        """
        Calcula y visualiza la matriz de correlación entre variables numéricas.
        
        Parameters:
        variables (List[str]): Lista de variables numéricas
        """
        corr_matrix = self.data[variables].corr()
        plt.figure(figsize=(10, 8))
        sns.heatmap(corr_matrix, annot=True, cmap='coolwarm', center=0)
        plt.title('Matriz de Correlación')
        plt.show()
        self.resultados['correlaciones'] = corr_matrix
        
    def prueba_hipotesis(self, var1: str, var2: str, test_type: str = 't_test'):
        """
        Realiza pruebas de hipótesis según el tipo especificado.
        
        Parameters:
        var1 (str): Primera variable
        var2 (str): Segunda variable
        test_type (str): Tipo de prueba ('t_test' o 'chi_square')
        """
        if test_type == 't_test':
            stat, pvalue = stats.ttest_ind(self.data[var1], self.data[var2])
            resultado = {'estadístico': stat, 'p_valor': pvalue}
        elif test_type == 'chi_square':
            contingency_table = pd.crosstab(self.data[var1], self.data[var2])
            stat, pvalue, dof, expected = stats.chi2_contingency(contingency_table)
            resultado = {
                'estadístico': stat,
                'p_valor': pvalue,
                'grados_libertad': dof
            }
        self.resultados[f'prueba_{test_type}_{var1}_{var2}'] = resultado
        return resultado
    
    def generar_reporte(self):
        """
        Genera un reporte resumido de todos los análisis realizados.
        """
        print("=== REPORTE DE ANÁLISIS ESTADÍSTICO ===\n")
        
        if 'estadistica_descriptiva' in self.resultados:
            print("\nEstadísticas Descriptivas:")
            print(self.resultados['estadistica_descriptiva'])
            
        if 'frecuencias' in self.resultados:
            print("\nTablas de Frecuencia:")
            for var, freq in self.resultados['frecuencias'].items():
                print(f"\nVariable: {var}")
                print(freq)
                
        if 'correlaciones' in self.resultados:
            print("\nCorrelaciones:")
            print(self.resultados['correlaciones'])
            
        # Imprimir resultados de pruebas de hipótesis
        for key in self.resultados:
            if key.startswith('prueba_'):
                print(f"\nResultados de {key}:")
                print(self.resultados[key])

# Ejemplo de uso
def ejemplo_analisis(data_path: str):
    """
    Ejemplo de cómo utilizar la clase para análisis.
    
    Parameters:
    data_path (str): Ruta al archivo de datos
    """
    # Cargar datos
    data = pd.read_csv(data_path)
    
    # Inicializar análisis
    analisis = AnalisisInnovacionDigital(data)
    
    # Realizar análisis descriptivo
    analisis.estadistica_descriptiva()
    
    # Análisis de frecuencias para variables categóricas
    vars_categoricas = ['nivel_acceso_tic', 'tipo_dispositivo']  # Ejemplo
    analisis.analisis_frecuencias(vars_categoricas)
    
    # Visualizaciones
    analisis.visualizar_distribucion('rendimiento_academico', 'Distribución del Rendimiento Académico')
    analisis.grafico_barras('nivel_acceso_tic', 'Nivel de Acceso a TIC')
    
    # Análisis de correlación
    vars_numericas = ['rendimiento_academico', 'horas_uso_tic', 'nivel_competencia']  # Ejemplo
    analisis.analisis_correlacion(vars_numericas)
    
    # Pruebas de hipótesis
    analisis.prueba_hipotesis('rendimiento_academico', 'horas_uso_tic', 't_test')
    
    # Generar reporte
    analisis.generar_reporte()
