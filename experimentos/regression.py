import pandas as pd
import scipy.stats as stats
import matplotlib.pyplot as plt
import numpy as np
from math import sqrt
import statsmodels.api as sm

class RegresionVelocidadDistancia:
    def __init__(self, filename):
        self.filename = filename
        
    def run(self):
        self.df = pd.read_excel(self.filename)
        self.df.rename(columns={'Distancia al nodo (km)': 'distancia', 'Velocidad de descarga (Mbps)': 'velocidad'}, inplace=True)
        
        x = self.df['distancia']
        y = self.df['velocidad']
        x = sm.add_constant(x) # Adds an intercept term to the simple linear regression formula
        lin_model = sm.OLS(y, x)
        regr_results = lin_model.fit()
        
        return regr_results
        

        # model = stats.linregress(df['distancia'], df['velocidad'])
        
        # return pd.DataFrame({
        #     "distancia": df['distancia'], 
        #     "velocidad": df['velocidad'], 
        #     "ajuste": model.intercept + model.slope * df['distancia'],
        #     "residuo": df['velocidad'] - (model.intercept + model.slope * df['distancia'])})
    
    def sct(self):
        results = self.run()
        return results.centered_tss
    
    def scr(self):
        results = self.run()
        return results.ess
    
    def sce(self):
        results = self.run()
        return results.ssr
    
    def n(self):
        results = self.run()
        return results.nobs
    
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
        res = self.run()
        plt.scatter(self.df['distancia'], res.resid)
        plt.axhline(0, color='green', linestyle='--')
        plt.xlabel('Distancia al nodo (km)')
        plt.ylabel('Residuo')
        plt.title('Gráfico de residuos')
        plt.show()
        
    def residuos_estandar(self):
        res = self.run()
        plt.scatter(self.df['distancia'], stats.zscore(res.resid))
        plt.axhline(0, color='green', linestyle='--')
        plt.axhline(2, color='red', linestyle='--')
        plt.axhline(-2, color='red', linestyle='--')
        plt.xlabel('Distancia al nodo (km)')
        plt.ylabel('Residuo estandarizado')
        plt.title('Gráfico de residuos estandarizados')
        plt.show()        
        
    def normalidad_residuos(self):
        res = self.run()
        stats.probplot(res.resid, dist="norm", plot=plt)
        plt.title('Gráfico Q-Q de residuos')
        plt.xlabel('Cuantiles teóricos')
        plt.ylabel('Cuantiles de residuos')

        plt.show()
        
    def residuos_histograma(self):
        res = self.run()
        plt.hist(res.resid, bins=int(sqrt(self.n())), edgecolor='black')
        plt.xlabel('Residuo')
        plt.ylabel('Frecuencia')
        plt.title('Histograma de residuos')
        plt.show()
        
    def residuos_test(self):
        res = self.run()
        stat, p = stats.shapiro(res.resid)
        return stat, p