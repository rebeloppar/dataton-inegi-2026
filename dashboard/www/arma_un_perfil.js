// Arma un perfil (decisión 72). Datos: window.PERFIL_DATOS, desde s2_perfil_prob.csv
(function(){
const OPC={
 sexo:{rot:"Sexo",v:["Mujer","Hombre"]},
 edad:{rot:"Edad",v:["18-24","25-34","35-44","45-54","55-64","65+"]},
 escolaridad:{rot:"Escolaridad",v:["Sin escolaridad","Preescolar o primaria","Secundaria","Media superior","Superior"],corto:["Sin escolaridad","Primaria","Secundaria","Media superior","Superior"]},
 localidad:{rot:"Tamaño de localidad",v:["Menos de 2,500","2,500 a 14,999","15 mil a 99,999","100 mil y más"],corto:["Menos de 2,500","2,500 a 15 mil","15 mil a 100 mil","100 mil o más"]},
 estrato:{rot:"Estrato",v:["Bajo","Medio bajo","Medio alto","Alto"]},
 indigena:{rot:"¿Se considera indígena?",v:["No","Sí"],clave:"indígena",extra:true},
 discapacidad:{rot:"¿Tiene alguna discapacidad?",v:["No","Sí"],clave:"discapacidad",extra:true}};
const DATOS=window.PERFIL_DATOS; const sel={}; let extra=false;
// Filtros activos: indígena y discapacidad solo si se activa el interruptor
const activas=()=>Object.keys(OPC).filter(k=>!OPC[k].extra||extra);
const ctrl=document.getElementById("ap-controles");
// Interruptor y caja para indígena y discapacidad (opcionales)
const caja=document.createElement("div");caja.className="ap-extra";
caja.innerHTML=`<label class="ap-switch"><input type="checkbox" id="ap-extra-on"><span class="ap-slider"></span>Agregar indígena y discapacidad</label>
<div class="ap-por-que"><b>¿Por qué van aparte?</b> Entre personas con la misma escolaridad, edad, localidad y estrato, considerarse indígena baja la probabilidad 1.9 puntos y tener discapacidad, 1.3 (no significativo). La brecha existe, pero pasa sobre todo por esas otras características: comparando solo a personas del mismo sexo y edad, la brecha indígena es de 12.9 puntos y la de discapacidad, de 4.0 (ver «La regresión paso a paso»). Con el interruptor apagado, la cifra promedia a todas las personas del perfil.</div>
<div class="ap-extra-grupos" hidden></div>`;
const grupos_extra=caja.querySelector(".ap-extra-grupos");
for(const k in OPC){const g=document.createElement("div");g.className="ap-grupo";
 g.innerHTML=`<div class="ap-rot">${OPC[k].rot}</div><div class="ap-pills"></div>`;
 OPC[k].v.forEach((v,i)=>{const b=document.createElement("button");b.className="ap-pill";b.dataset.k=k;b.dataset.v=v;b.textContent=(OPC[k].corto||OPC[k].v)[i];
  b.onclick=()=>{sel[k]=v;g.querySelectorAll(".ap-pill").forEach(x=>x.classList.remove("on"));b.classList.add("on");pinta();};
  g.querySelector(".ap-pills").appendChild(b);});
 (OPC[k].extra?grupos_extra:ctrl).appendChild(g);}
ctrl.appendChild(caja);
caja.querySelector("#ap-extra-on").onchange=e=>{extra=e.target.checked;grupos_extra.hidden=!extra;
 if(!extra){delete sel.indigena;delete sel.discapacidad;grupos_extra.querySelectorAll(".ap-pill.on").forEach(x=>x.classList.remove("on"));}
 pinta();};
// Botón para borrar todas las selecciones y volver a la figura vacía
const limpiar=document.createElement("button");limpiar.className="ap-limpiar";limpiar.type="button";
limpiar.textContent="Borrar todos los filtros";
limpiar.onclick=()=>{for(const k in sel)delete sel[k];ctrl.querySelectorAll(".ap-pill.on").forEach(x=>x.classList.remove("on"));pinta();};
ctrl.appendChild(limpiar);

const PELO=["#3A2A20","#3A2A20","#4A3A30","#6E6259","#A39C95","#DCD9D4"];
function figura(){
 const s=sel.sexo, e=sel.edad?OPC.edad.v.indexOf(sel.edad):-1;
 if(!s && e<0){return `<g fill="#E5E7EB"><circle cx="120" cy="88" r="44"/><path d="M48 290 Q48 168 120 158 Q192 168 192 290Z"/></g>
  <text x="120" y="104" text-anchor="middle" font-size="44" fill="#9CA3AF" font-weight="700">?</text>`;}
 const piel="#E7B48F", pelo=e<0?"#5B4A3F":PELO[e], ropa="#3E9A66", ropaSomb="#337F54";
 const mujer=s==="Mujer", neutro=!s;
 const enc = e>=5 ? 6 : (e>=4 ? 3 : 0);           // ligera inclinación en edades mayores
 let g=`<g transform="translate(0 ${enc})">`;
 // pelo largo detrás (mujer)
 if(mujer) g+=`<path d="M70 92 Q68 40 120 36 Q172 40 170 92 L176 170 Q120 182 64 170Z" fill="${pelo}"/>`;
 // cuerpo
 g+=`<path d="M46 290 Q46 176 120 164 Q194 176 194 290Z" fill="${neutro?'#D1D5DB':ropa}"/>`;
 if(mujer) g+=`<path d="M100 166 Q120 190 140 166" fill="none" stroke="${ropaSomb}" stroke-width="5" stroke-linecap="round"/>`;
 else if(!neutro) g+=`<path d="M106 166 L120 186 L134 166" fill="${ropaSomb}"/>`;
 // cuello y cabeza
 g+=`<rect x="108" y="128" width="24" height="36" rx="8" fill="${piel}"/>`;
 g+=`<ellipse cx="120" cy="92" rx="40" ry="46" fill="${piel}"/>`;
 // pelo frente
 if(mujer) g+=`<path d="M78 84 Q82 44 120 42 Q160 44 162 84 Q140 66 120 64 Q98 66 78 84Z" fill="${pelo}"/>`;
 else if(!neutro){ if(e>=5) g+=`<path d="M80 80 Q78 56 92 52 Q86 70 88 82Z M160 80 Q162 56 148 52 Q154 70 152 82Z" fill="${pelo}"/>`;
   else g+=`<path d="M78 82 Q78 44 120 42 Q162 44 162 82 Q150 60 120 60 Q92 60 78 82Z" fill="${pelo}"/>`;}
 else g+=`<path d="M80 80 Q82 46 120 44 Q158 46 160 80 Q140 62 120 62 Q100 62 80 80Z" fill="${pelo}"/>`;
 // cara
 g+=`<circle cx="104" cy="94" r="4" fill="#1F2937"/><circle cx="136" cy="94" r="4" fill="#1F2937"/>`;
 g+=`<path d="M106 114 Q120 124 134 114" fill="none" stroke="#1F2937" stroke-width="3" stroke-linecap="round"/>`;
 if(e>=3) g+=`<path d="M92 104 q4 3 8 0 M140 104 q4 3 8 0" fill="none" stroke="#C98F6A" stroke-width="1.6" stroke-linecap="round"/>`;
 if(e>=4) g+=`<g fill="none" stroke="#374151" stroke-width="2.5"><circle cx="104" cy="94" r="11"/><circle cx="136" cy="94" r="11"/><path d="M115 94 h10 M93 92 l-12 -4 M147 92 l12 -4"/></g>`;
 if(e>=5) g+=`<path d="M92 82 q12 -6 24 0 M124 82 q12 -6 24 0" fill="none" stroke="${pelo}" stroke-width="4" stroke-linecap="round"/>`;
 g+=`</g>`;
 return g;}

function insEsc(i){ // libros apilados: 0 a 4
 let s=`<svg viewBox="0 0 52 44">`; if(i<0) return s+`<rect x="8" y="32" width="36" height="8" rx="2" fill="#E5E7EB"/></svg>`;
 const cols=["#B97A1C","#3E9A66","#0F72A8","#6A4C93"];
 if(i===0) s+=`<rect x="8" y="32" width="36" height="8" rx="2" fill="none" stroke="#9CA3AF" stroke-dasharray="3 2"/>`;
 for(let k=0;k<i;k++) s+=`<rect x="${8+(k%2)*3}" y="${32-k*8}" width="34" height="7" rx="2" fill="${cols[k]}"/>`;
 if(i===4) s+=`<path d="M26 2 L44 8 L26 14 L8 8Z" fill="#1F2937"/><path d="M40 9 v8" stroke="#1F2937" stroke-width="2"/>`;
 return s+`</svg>`;}
function insLoc(i){
 let s=`<svg viewBox="0 0 52 44">`; const c="#6B7280", f=i<0?"#E5E7EB":"#4B5563";
 const casa=(x,y,w)=>`<path d="M${x} ${y} l${w/2} -${w/2} l${w/2} ${w/2} v${w*.7} h-${w}Z" fill="${f}"/>`;
 if(i<=0) s+=casa(18,26,16);
 else if(i===1) s+=casa(6,28,14)+casa(20,24,14)+casa(34,28,12);
 else if(i===2) s+=casa(4,30,12)+`<rect x="18" y="14" width="12" height="26" fill="${f}"/>`+casa(32,28,14);
 else s+=`<rect x="4" y="18" width="10" height="22" fill="${f}"/><rect x="16" y="6" width="12" height="34" fill="${f}"/><rect x="30" y="12" width="9" height="28" fill="${f}"/><rect x="41" y="22" width="8" height="18" fill="${f}"/>`;
 return s+`<path d="M2 41 h48" stroke="${c}" stroke-width="2"/></svg>`;}
function insEst(i){
 let s=`<svg viewBox="0 0 52 44">`;
 for(let k=0;k<4;k++){const on=i>=0&&k<=i; s+=`<rect x="${4+k*12}" y="${30-k*7}" width="9" height="${10+k*7}" rx="2" fill="${on?'#B97A1C':'#E5E7EB'}"/>`;}
 return s+`</svg>`;}

function insInd(i){ // marca de identidad (autoadscripción)
 const c=i===1?"#3E9A66":(i===0?"#9CA3AF":"#E5E7EB");
 let s=`<svg viewBox="0 0 52 44"><circle cx="26" cy="22" r="19" fill="${i===1?'#E3F1E8':'none'}" stroke="${c}" stroke-width="2"/>`;
 s+=`<g fill="none" stroke="${c}" stroke-width="2.2" stroke-linecap="round">
  <path d="M18 30 Q16 20 22 15 Q28 11 33 16"/><path d="M22 32 Q20 23 24 19 Q28 16 31 20 Q33 24 31 30"/>
  <path d="M26 33 Q24 26 26 23 Q28 22 28 26 Q29 30 27 34"/><path d="M36 21 Q37 26 35 31"/></g>`;
 return s+`</svg>`;}
function insDis(i){ // pictograma de persona, sin silla de ruedas
 const c=i===1?"#3E9A66":(i===0?"#9CA3AF":"#E5E7EB");
 let s=`<svg viewBox="0 0 52 44"><circle cx="26" cy="22" r="19" fill="${i===1?'#E3F1E8':'none'}" stroke="${c}" stroke-width="2"/>`;
 s+=`<circle cx="26" cy="13" r="4" fill="${c}"/><path d="M26 18 v11 M18 22 h16 M26 29 l-5 7 M26 29 l5 7" stroke="${c}" stroke-width="2.6" stroke-linecap="round" fill="none"/>`;
 return s+`</svg>`;}

// --- Perfiles sugeridos para comparar -----------------------------------------
// Cada par cambia una sola característica (o, en «Los extremos», todas), para
// ver qué pesa más. Se pueden editar aquí; las claves son las de OPC.
const base1={sexo:"Hombre",edad:"35-44",escolaridad:"Secundaria",localidad:"15 mil a 99,999",estrato:"Medio bajo"};
const SUGERIDOS=[
 {titulo:"Los extremos",perfiles:[
  {nombre:"Mujer de 65+ con primaria, rural",s:{sexo:"Mujer",edad:"65+",escolaridad:"Preescolar o primaria",localidad:"Menos de 2,500",estrato:"Bajo"}},
  {nombre:"Mujer de 25-34 con universidad, ciudad grande",s:{sexo:"Mujer",edad:"25-34",escolaridad:"Superior",localidad:"100 mil y más",estrato:"Alto"}}]},
 {titulo:"Solo cambia la escolaridad",nota:"Hombre, 35-44, ciudad media, estrato medio bajo",perfiles:[
  {nombre:"Con secundaria",s:base1},
  {nombre:"Con universidad",s:{...base1,escolaridad:"Superior"}}]},
 {titulo:"Solo cambia la edad",nota:"Mujer con secundaria, ciudad grande, estrato medio bajo",perfiles:[
  {nombre:"De 25-34",s:{sexo:"Mujer",edad:"25-34",escolaridad:"Secundaria",localidad:"100 mil y más",estrato:"Medio bajo"}},
  {nombre:"De 65+",s:{sexo:"Mujer",edad:"65+",escolaridad:"Secundaria",localidad:"100 mil y más",estrato:"Medio bajo"}}]},
 {titulo:"Solo cambia la localidad",nota:"Hombre, 45-54, primaria, estrato bajo",perfiles:[
  {nombre:"Rural (menos de 2,500)",s:{sexo:"Hombre",edad:"45-54",escolaridad:"Preescolar o primaria",localidad:"Menos de 2,500",estrato:"Bajo"}},
  {nombre:"Ciudad de 100 mil o más",s:{sexo:"Hombre",edad:"45-54",escolaridad:"Preescolar o primaria",localidad:"100 mil y más",estrato:"Bajo"}}]},
 {titulo:"Solo cambia el sexo",nota:"35-44, secundaria, ciudad media, estrato medio bajo",perfiles:[
  {nombre:"Mujer",s:{...base1,sexo:"Mujer"}},
  {nombre:"Hombre",s:base1}]}];
const CINCO=["sexo","edad","escolaridad","localidad","estrato"];
const probDe=s=>{const d=DATOS[[...CINCO.map(k=>s[k]),"Todos","Todos"].join("|")];return d?Math.round(d[0]):null;};
function aplica(s){
 if(extra){extra=false;caja.querySelector("#ap-extra-on").checked=false;grupos_extra.hidden=true;delete sel.indigena;delete sel.discapacidad;}
 ctrl.querySelectorAll(".ap-pill.on").forEach(x=>x.classList.remove("on"));
 CINCO.forEach(k=>{sel[k]=s[k];const b=ctrl.querySelector(`.ap-pill[data-k="${k}"][data-v="${s[k]}"]`);if(b)b.classList.add("on");});
 pinta();}
const sug=document.getElementById("ap-sugeridos");
function pintaSugeridos(){
 if(!sug)return;
 const actual=!extra&&CINCO.every(k=>sel[k])?CINCO.map(k=>sel[k]).join("|"):null;
 sug.innerHTML=`<div class="ap-lab">Perfiles para comparar</div><div class="ap-nota" style="margin-top:0">Haz clic en uno para verlo.</div>`+
  SUGERIDOS.map((g,gi)=>`<div class="ap-sg"><div class="ap-sg-tit">${g.titulo}</div>${g.nota?`<div class="ap-sg-nota">${g.nota}</div>`:""}`+
   g.perfiles.map((p,pi)=>`<button type="button" class="ap-sg-btn${actual===CINCO.map(k=>p.s[k]).join("|")?" on":""}" data-g="${gi}" data-p="${pi}"><span>${p.nombre}</span><b>${probDe(p.s)} %</b></button>`).join("")+`</div>`).join("");
 sug.querySelectorAll(".ap-sg-btn").forEach(b=>b.onclick=()=>aplica(SUGERIDOS[b.dataset.g].perfiles[b.dataset.p].s));}

function pinta(){
 pintaSugeridos();
 document.getElementById("ap-fig").innerHTML=figura();
 const iE=sel.escolaridad?OPC.escolaridad.v.indexOf(sel.escolaridad):-1;
 const iL=sel.localidad?OPC.localidad.v.indexOf(sel.localidad):-1;
 const iS=sel.estrato?OPC.estrato.v.indexOf(sel.estrato):-1;
 document.getElementById("ap-ins").innerHTML=
  `<div class="ap-ins">${insEsc(iE)}${iE<0?"Escolaridad":OPC.escolaridad.corto[iE]}</div>`+
  `<div class="ap-ins">${insLoc(iL)}${iL<0?"Localidad":OPC.localidad.corto[iL]}</div>`+
  `<div class="ap-ins">${insEst(iS)}${iS<0?"Estrato":"Estrato "+sel.estrato.toLowerCase()}</div>`+
  (!extra?"":`<div class="ap-ins">${insInd(sel.indigena?OPC.indigena.v.indexOf(sel.indigena):-1)}${!sel.indigena?"Indígena":(sel.indigena==="Sí"?"Se considera indígena":"No indígena")}</div>`+
  `<div class="ap-ins">${insDis(sel.discapacidad?OPC.discapacidad.v.indexOf(sel.discapacidad):-1)}${!sel.discapacidad?"Discapacidad":(sel.discapacidad==="Sí"?"Con discapacidad":"Sin discapacidad")}</div>`);
 const falta=activas().filter(k=>!sel[k]);
 const r=document.getElementById("ap-res");
 if(falta.length){r.innerHTML=`<div class="ap-vacio">Elige ${(l=>l.length>1?l.slice(0,-1).join(", ")+" y "+l[l.length-1]:l[0])(falta.map(k=>OPC[k].clave||OPC[k].rot.toLowerCase()))} para ver la probabilidad.</div>`;return;}
 const rest=[sel.localidad,sel.estrato,...(extra?[sel.indigena,sel.discapacidad]:["Todos","Todos"])];
 const k=[sel.sexo,sel.edad,sel.escolaridad,...rest].join("|"), d=DATOS[k];
 const ks=[sel.sexo,sel.edad,"Superior",...rest].join("|"), ds=DATOS[ks];
 const ko=[sel.sexo==="Mujer"?"Hombre":"Mujer",sel.edad,sel.escolaridad,...rest].join("|"), dd=DATOS[ko];
 r.innerHTML=`<div class="ap-lab">Probabilidad de comprar en línea</div><div class="ap-num">${Math.round(d[0])} %</div>
  <div class="ap-barra"><div class="ap-ic" style="left:${d[1]}%;width:${d[2]-d[1]}%"></div><div class="ap-pt" style="left:calc(${d[0]}% - 2px)"></div><div class="ap-nac" style="left:43.7%"></div></div>
  <div class="ap-esc"><span>0 %</span><span>100 %</span></div>
  <div class="ap-nota">Intervalo al 90 %: ${d[1]} a ${d[2]} %.</div>
  <div class="ap-comp">${sel.escolaridad!=="Superior"?`Con educación superior: <b>${Math.round(ds[0])} %</b><br>`:""}Si fuera ${sel.sexo==="Mujer"?"hombre":"mujer"}: <b>${Math.round(dd[0])} %</b><br>Promedio nacional: <b>44 %</b></div>
  ${d[3]<5?`<div class="ap-aviso">Pocas personas así en la encuesta (${d[3]}): el modelo extrapola, tomar con cautela.</div>`:""}`;}
if(document.readyState==='loading'){document.addEventListener('DOMContentLoaded',pinta);}else{pinta();}
})();
