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

    def atipicos(self):
        df = self.run()
        n = self.n()
        x = df['distancia']
        s = sqrt(self.sce() / (n - 2))
        sxx = ((x - x.mean()) ** 2).sum()

        # apalancamiento: cuanto tira cada punto segun su posicion en x
        h = 1 / n + (x - x.mean()) ** 2 / sxx
        r_est = df['residuo'] / (s * np.sqrt(1 - h))
        cook = (r_est ** 2 / 2) * (h / (1 - h))

        return pd.DataFrame({
            'distancia': x,
            'velocidad': df['velocidad'],
            'predicho': df['ajuste'].round(2),
            'residuo': df['residuo'].round(2),
            'residuo_estandarizado': r_est.round(2),
            'apalancamiento': h.round(4),
            'cook': cook.round(4)})

    def linealidad(self):
        df = self.run()
        n = self.n()
        x, y = df['distancia'], df['velocidad']

        # ajustamos una parabola y vemos si baja lo suficiente la suma de
        # cuadrados del error como para justificar el termino de mas
        coeficientes = np.polyfit(x, y, 2)
        sce_cuadratico = ((y - np.polyval(coeficientes, x)) ** 2).sum()

        F = (self.sce() - sce_cuadratico) / (sce_cuadratico / (n - 3))
        p = stats.f.sf(F, 1, n - 3)
        return F, p

    def breusch_pagan(self):
        df = self.run()
        n = self.n()

        # regresion de los residuos al cuadrado sobre x: si la varianza fuera
        # constante, x no deberia explicar nada de e^2
        e2 = df['residuo'] ** 2
        ajuste = stats.linregress(df['distancia'], e2)
        LM = n * ajuste.rvalue ** 2
        p = stats.chi2.sf(LM, 1)
        return LM, p