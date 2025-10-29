# Visor Meteo‑Energético

Aplicación web para visualizar, en tiempo real y de forma progresiva, predicciones **meteorológicas** (temperatura) y **energéticas** (energía producida) a partir de un archivo **YAML**. Incluye KPIs del último valor recibido, lista de últimos cambios, gráfico por **minuto** y selector de **tema**.

> Tecnologías: **Web Components (Lit)**, **TypeScript**, **uPlot**, **js‑yaml**, **CSS variables** y **Rspack**.

---

## Características

- **Reproducción progresiva** de los datos cada **5 s** (sin recargar).
- **Sincronización a la hora actual** al iniciar.
- **Gráfico** de temperatura (°C) y energía (kWh) con **media por minuto** y ejes Y independientes.
- **Mostrar/ocultar series** desde la leyenda.
- **Selector de tema** (claro, oscuro y sunset) sin recargar.
- Accesible: roles ARIA, navegación por teclado y foco visible.
- Conversión automática de **unidades**: `K`/`dK` → `°C`, `Wh` → `kWh`.

---

## Estructura del proyecto

public/
  styles/base.css             # Layout y componentes (sin colores)
  themes/{dark,ocean,sunset}.css # Variables de color (temas)
  data.yml                    # Fuente de datos
index.html                    # Shell + toolbar (selector de tema)
src/
  lib/components/meteo-dashboard.ts  # Web Component principal
  main.ts                             # Bootstrap del componente
rspack.config.js
tsconfig.json
package.json

---

## Requisitos

- **Node.js 18+**
- Gestor de paquetes: recomendado **pnpm** (también funciona con `npm`/`yarn`).

---

## Instalación y ejecución

### 1) Instalar dependencias

```bash
# con pnpm
pnpm install

# o con npm
npm install
```

### 2) Desarrollo

```bash
pnpm dev
# La consola indicará la URL (p.ej. http://localhost:5174)
```

### 3) Build de producción

```bash
pnpm build           # genera /dist
pnpm preview         # sirve la build para verificación local
```

> También puedes servir el `index.html` y la carpeta `public/` con cualquier servidor estático simple si lo prefieres.

---

## Formato de `public/data.yml`

La app admite tanto claves en inglés como en español y diferentes unidades:

```yml
temperature: # o "temperatura"
  unit: K # admite: C | K | dK
  values:
    - { time: "20:14:35", value: 295.75 }
    - { time: "20:14:40", value: 295.73 }
    # ... (HH:MM:SS)

energy: # o "energia" | "power" | "potencia"
  unit: kWh # admite: kWh | Wh
  values:
    - { time: "20:14:35", value: 71.14 }
    - { time: "20:14:40", value: 71.13 }
```


- Los arrays vienen **ordenados** por tiempo (`HH:MM:SS`). Si no lo estuvieran, la app los ordena.
- Si `energy` no existe, la aplicación funciona sólo con temperatura.
- Conversión de unidades:
  - `K` → `°C = K − 273.15`
  - `dK` → `°C = (dK / 10) − 273.15`
  - `Wh` → `kWh = Wh / 1000`

---

## Uso dentro de la UI

- La barra superior muestra el **progreso** (dato actual/total) y el **selector de tema**.
- En la izquierda:
  - **KPIs** con último valor de Temperatura y Energía.
  - **Cuenta atrás** hasta la próxima actualización (5 s).
  - **Últimos 5 cambios** de cada serie.
- A la derecha:
  - **Gráfico** de medias por **minuto**.
  - Botones para **mostrar/ocultar** cada serie.

Atajos: **Barra espaciadora** pausa/reanuda la reproducción.

---

## Temas

Los colores se definen en `public/themes/*.css` como **variables CSS** (`--c-temp`, `--c-energy`, `--bg`, etc.). Cambiar el tema actualiza los colores del gráfico en caliente (sin recrear toda la vista).

---

## Decisiones técnicas

- **Lit + Web Components**: estándar nativo, ligero y portable a cualquier stack.
- **uPlot**: librería de gráficos muy rápida y pequeña, ideal para líneas/puntos.
- **js-yaml**: parseo robusto del YAML.
- **TypeScript**: tipado de modelos y utilidades (SeriesPoint, conversión unidades).
- **Rspack**: builder rápido (Rust), DX fluida en dev/build.

---

---

## Scripts útiles

```bash
pnpm dev      # desarrollo con servidor local
pnpm build    # build de producción (dist/)
pnpm preview  # sirve dist/ para validar
```

Con `npm`:

```bash
npm run dev
npm run build
npm run preview
```

---

## Solución de problemas

- **No carga el YAML**: revisa la ruta `public/data.yml` y desactiva caché (la app usa `cache: "no-store"`).
- **Gráfico vacío**: asegúrate de que hay datos con `time` en formato `HH:MM:SS`.
- **Colores raros**: comprueba que el tema activo define `--c-temp`/`--c-energy` y que el `<link id="theme">` apunta al CSS correcto.

---

## Licencia

MIT — usa, modifica y comparte libremente.
