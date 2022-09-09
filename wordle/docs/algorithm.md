# Explicación del algoritmo

El objetivo del algoritmo es minimizar el número promedio de turnos para conseguir la victoria. 

Una búsqueda exhaustiva, por fuerza bruta, sería computacionalmente muy costosa. Por ello, se acota la búsqueda usando como heurística, en cada nivel, las palabras que producen una reducción esperada mayor de la entropía. 

Esta heurística no es perfecta (es fácil encontrar ejemplos en que la opción que minimiza la entropía es subóptima). Pero hay cierta correlación entre ambas cosas y por tanto el algoritmo logra un buen resultado (en cada nodo, se exploran a fondo los 20 candidatos que más reducen la entropía). 
