# Visor Meteo-Energético

Aplicación web para visualizar, en tiempo real y de forma progresiva, predicciones **meteorológicas** (temperatura) y **energéticas** (energía producida)** a partir de un archivo YAML.** Incluye KPIs del último valor recibido, lista de recientes, gráfico por minuto y selector de tema.

> Tecnologías: **Web Components (Lit)**, **TypeScript**, **uPlot**, **js-yaml**, **CSS variables** y **Rspack**.

---

## Características

 - Actualización automática cada **5 segundos**
 - Sincronización con la hora real al iniciar
 - Gráfico con **media por minuto** (°C / kWh)
 - Mostrar/ocultar series desde la interfaz
 -  **Temas**: Claro, Oscuro, Sunset (CSS variables)
 - Conversión de unidades: `K`/`dK` → `°C`, `Wh` → `kWh`

---

## Estructura del proyecto

```txt
public/
 ├─ styles/base.css
 ├─ themes/{dark,ocean,sunset}.css
 ├─ data.yml
index.html
src/
 ├─ lib/components/meteo-dashboard.ts
 └─ main.ts
rspack.config.js
tsconfig.json
package.json
```

---

## Requisitos

- Node.js **18+**
- Gestor recomendado: **pnpm**
  > También funciona con `npm` o `yarn`

---

## Instalación y ejecución local

### 1) Instalar dependencias

```bash
pnpm install
# o
npm install
```

### 2) Ejecutar en desarrollo

```bash
pnpm dev
```

### 3) Compilar para producción

```bash
pnpm build
pnpm preview
```

> También puede ejecutarse sirviendo `index.html` + `public/` en un servidor estático simple.

---

## Formato de `public/data.yml`

```yml
temperature: # o "temperatura"
  unit: K # C | K | dK
  values:
    - { time: "20:14:35", value: 295.75 }
    - { time: "20:14:40", value: 295.73 }

energy: # o "energia" | "power" | "potencia"
  unit: kWh # o Wh
  values:
    - { time: "20:14:35", value: 71.14 }
    - { time: "20:14:40", value: 71.13 }
```

Ordenado por `HH:MM:SS`  
Soporta faltas de energía (modo solo temperatura)  
Conversión automática °C / kWh

---

## Uso en la interfaz

- Barra superior → progreso + selección de tema
- Lateral → KPI de temperatura y energía + últimos valores + contador
- Panel principal → gráfico interactivo

---

## Temas

Definidos mediante variables CSS en:

```
public/themes/
```

Cambio en tiempo real sin recargar la página.

---

## Decisiones técnicas

- **Lit + Web Components** → ligero, portable, estándar web
- **uPlot** → rendimiento óptimo en series grandes
- **js-yaml** → lectura de YAML directa y segura
- **TypeScript** → tipado fuerte y claridad
- **Rspack** → bundler rápido y moderno

---

## Scripts

```bash
pnpm dev
pnpm build
pnpm preview
```

Con npm:

```bash
npm run dev
npm run build
npm run preview
```

---

## Solución de problemas

| Problema         | Solución                               |
| ---------------- | -------------------------------------- |
| No carga el YAML | Verificar ruta relativa `./data.yml`   |
| Gráfico vacío    | Asegurar formato `HH:MM:SS` en tiempos |
| Tema no cambia   | Comprobar `link#theme` y variables CSS |

---

## Despliegue en GitHub Pages

🔗 **Demo online:**  
https://irenefpdaw.github.io/meteo-dashboard/

### Pasos realizados

1. Crear carpeta `docs/` y copiar dentro:
   ```
   index.html
   data.yml
   styles/
   themes/
   ```
2. Inicializar Git y primer commit:
   ```bash
   git init
   git add .
   git commit -m "Publicación GitHub Pages"
   ```
3. Conectar repo y subir:
   ```bash
   git remote add origin https://github.com/ireneFPdaw/meteo-dashboard.git
   git branch -M main
   git push -u origin main
   ```
4. Activar Pages:
   - Settings → Pages
   - Source: _Deploy from a branch_
   - Branch: `main`
   - Folder: `/docs`

El sitio queda publicado automáticamente.

Actualización futura:

```bash
git add .
git commit -m "Update"
git push
```

---

## Licencia

**MIT** — libre uso y modificación
