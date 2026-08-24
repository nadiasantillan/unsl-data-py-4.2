import pandas as pd
import scipy.stats as stats
import matplotlib.pyplot as plt
import numpy as np
from math import sqrt

class RegresionVelocidadDistancia:
    def __init__(self, filename):
        self.filename = filename
        
    def run(self):
        df = pd.read_excel(self.filename)
        df.rename(columns={'Distancia al nodo (km)': 'distancia', 'Velocidad de descarga (Mbps)': 'velocidad'}, inplace=True)
        model = stats.linregress(df['distancia'], df['velocidad'])
        
        return pd.DataFrame({
            "distancia": df['distancia'], 
            "velocidad": df['velocidad'], 
            "ajuste": model.intercept + model.slope * df['distancia'],
            "residuo": df['velocidad'] - (model.intercept + model.slope * df['distancia'])})
    
    def sct(self):
        df = self.run()
        return sum((df['velocidad'] - df['velocidad'].mean()) ** 2)
    
    def scr(self):
        df = self.run()
        return sum((df['ajuste'] - df['velocidad'].mean()) ** 2)
    
    def sce(self):
        df = self.run()
        return sum((df['velocidad'] - df['ajuste']) ** 2)
    def n(self):
        df = self.run()
        return len(df)
    
    def f(self):

        return (self.scr() / 1) / (self.sce() / (self.n() - 2))
    
    def f_chart(self):
        seq = np.linspace(self.f()-10, self.f() + 10, 100)
        scipy_f = stats.f.pdf(seq, 1, self.n() - 2)
        plt.plot(seq, scipy_f)
        plt.fill_between(seq, scipy_f, where=((seq >= self.f()) & (seq <= self.f()+10)), color="blue", alpha=0.5)
        plt.xlabel('F')
        plt.ylabel('Densidad de Probabilidad')
        plt.title(f'Distribución F(1, {self.n() - 2}) - valor p: {stats.f.sf(self.f(), 1, self.n() - 2):.4E}')
        plt.show()
        
    def residuos(self):
        df = self.run()
        plt.scatter(df['distancia'], df['residuo'])
        plt.axhline(0, color='green', linestyle='--')
        plt.xlabel('Distancia al nodo (km)')
        plt.ylabel('Residuo')
        plt.title('Gráfico de residuos')
        plt.show()
        
    def residuos_estandar(self):
        df = self.run()
        plt.scatter(df['distancia'], stats.zscore(df['residuo']))
        plt.axhline(0, color='green', linestyle='--')
        plt.axhline(2, color='red', linestyle='--')
        plt.axhline(-2, color='red', linestyle='--')
        plt.xlabel('Distancia al nodo (km)')
        plt.ylabel('Residuo estandarizado')
        plt.title('Gráfico de residuos estandarizados')
        plt.show()        
        
    def normalidad_residuos(self):
        df = self.run()
        stats.probplot(df['residuo'], dist="norm", plot=plt)
        plt.title('Gráfico Q-Q de residuos')
        plt.xlabel('Cuantiles teóricos')
        plt.ylabel('Cuantiles de residuos')

        plt.show()
        
    def residuos_histograma(self):
        df = self.run()
        plt.hist(df['residuo'], bins=int(sqrt(self.n())), edgecolor='black')
        plt.xlabel('Residuo')
        plt.ylabel('Frecuencia')
        plt.title('Histograma de residuos')
        plt.show()
        
    def residuos_test(self):
        df = self.run()
        stat, p = stats.shapiro(df['residuo'])
        return stat, p