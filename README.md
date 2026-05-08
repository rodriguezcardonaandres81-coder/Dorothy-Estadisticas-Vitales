# Dorothy — Estadísticas Vitales (Nacimientos)
**Autor:** Andres Rodriguez © 2025  
**Stack:** R + Shiny + Leaflet + Plotly  
**Despliegue:** GitHub + Render.com (gratuito, sin límite de visitas)

---

## Estructura del repositorio

```
Dorothy_App/
├── app.R           ← Aplicación principal
├── packages.R      ← Instalación automática de paquetes
├── Dockerfile      ← Contenedor Docker para Render
├── render.yaml     ← Configuración de Render
└── README.md       ← Este archivo
```

---

## Paso 1 — Subir a GitHub

1. Ve a [github.com](https://github.com) e inicia sesión
2. Haz clic en **"New repository"**
3. Nómbralo `Dorothy-Estadisticas-Vitales` (o como prefieras)
4. Márcalo como **Público** (necesario para el plan gratuito de Render)
5. Haz clic en **"Create repository"**
6. Sube los 4 archivos:
   - `app.R`
   - `packages.R`
   - `Dockerfile`
   - `render.yaml`

   Puedes arrastrarlos directamente desde el navegador en GitHub o usar Git:
   ```bash
   git init
   git add .
   git commit -m "Primera versión Dorothy"
   git branch -M main
   git remote add origin https://github.com/TU_USUARIO/Dorothy-Estadisticas-Vitales.git
   git push -u origin main
   ```

---

## Paso 2 — Desplegar en Render.com

1. Ve a [render.com](https://render.com) y crea una cuenta gratuita
2. Haz clic en **"New +"** → **"Web Service"**
3. Conecta tu cuenta de GitHub
4. Selecciona el repositorio `Dorothy-Estadisticas-Vitales`
5. Render detectará el `Dockerfile` automáticamente
6. Configura:
   - **Name:** `dorothy-app`
   - **Region:** Oregon (US West) o la más cercana
   - **Branch:** `main`
   - **Plan:** `Free`
7. Haz clic en **"Create Web Service"**
8. Espera ~5-10 minutos mientras instala los paquetes de R
9. ¡Listo! Render te dará una URL como `https://dorothy-app.onrender.com`

---

## Credenciales de acceso

| Usuario   | Contraseña    | Permisos  |
|-----------|---------------|-----------|
| Andres    | 123           | Completo  |
| Norha     | 1989          | Completo  |
| invitado  | invitado123   | Lectura   |

⚠️ **Recomendación:** Cambia las contraseñas antes de publicar el repositorio, 
especialmente si el repositorio es público.

---

## Notas importantes

- **Plan gratuito de Render:** La app "duerme" después de 15 minutos de inactividad.
  Al volver a acceder tarda ~30 segundos en despertar. Esto es normal.
- **Sin límite de visitas:** A diferencia de Shinyapps.io, no hay restricción de horas de uso.
- **Datos:** Los archivos XLS y shapefiles se cargan en cada sesión — no se almacenan en el servidor.
- **Actualizaciones:** Cada vez que hagas `git push` a GitHub, Render redesplegará automáticamente.

---

## Solución de problemas comunes

| Problema | Solución |
|----------|----------|
| La app no carga el mapa | Verificar que el .zip del shapefile contenga todos los archivos (.shp, .dbf, .prj, .shx) |
| Error en columna 'Área Residencia' | Verificar que el Excel tenga exactamente esa columna con tilde |
| La app tarda mucho en arrancar | Normal en plan gratuito — el primer arranque instala paquetes (~5 min) |
| Error de locale en mes | Render usa locale en inglés; si los meses no aparecen bien, usar `locale = "C"` en `month()` |
