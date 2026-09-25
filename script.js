document.addEventListener('DOMContentLoaded', async () => {
  const ORCID_ID = '0009-0004-0483-0463';
  const YOUR_SURNAME = 'Esposito';

  const list = document.getElementById('publications-list');
  const status = document.getElementById('publications-status');

  if (!list || !status) return;

  // Gestione numero di pubblicazioni: 3 nella Home, tutte nella pagina Research
  const isHome = document.title.includes('Home');
  const numRows = isHome ? 3 : 50;

  try {
    // 1. Legge ESATTAMENTE e SOLO il tuo profilo ORCID pubblico (nessun algoritmo esterno, niente omonimi)
    const orcidRes = await fetch(
      `https://pub.orcid.org/v3.0/${ORCID_ID}/works`,
      {
        headers: { Accept: 'application/json' },
      }
    );

    if (!orcidRes.ok) throw new Error('ORCID API error');

    const orcidData = await orcidRes.json();
    let groups = orcidData.group || [];

    if (groups.length === 0) {
      status.textContent = 'No publications found on ORCID.';
      return;
    }

    // Ordina per anno (dal più recente al più vecchio)
    groups.sort((a, b) => {
      const yearA = parseInt(
        a['work-summary'][0]['publication-date']?.year?.value || '0'
      );
      const yearB = parseInt(
        b['work-summary'][0]['publication-date']?.year?.value || '0'
      );
      return yearB - yearA;
    });

    // Taglia la lista a seconda della pagina
    groups = groups.slice(0, numRows);
    status.style.display = 'none';

    // 2. Costruisce le card prendendo i DOI dalla tua lista e formattando i nomi
    for (const group of groups) {
      const summary = group['work-summary'][0];
      let title = summary.title?.title?.value || 'Untitled';
      let journal = summary['journal-title']?.value || '';
      let year = summary['publication-date']?.year?.value || '';

      let authorsText = `<strong>${YOUR_SURNAME}</strong>`; // Fallback base

      // Cerca il DOI nel tuo paper
      let doi = '';
      const extIds = summary['external-ids']?.['external-id'] || [];
      const doiObj = extIds.find((id) => id['external-id-type'] === 'doi');

      if (doiObj) {
        doi = doiObj['external-id-value'];

        // Chiede a Crossref solo la formattazione di questo specifico DOI
        try {
          const crRes = await fetch(`https://api.crossref.org/works/${doi}`);
          if (crRes.ok) {
            const crData = await crRes.json();
            const item = crData.message;

            title = item.title?.[0] || title;
            journal = item['container-title']?.[0] || journal;

            if (item.author) {
              authorsText = item.author
                .map((a) => {
                  const name = [a.family, a.given].filter(Boolean).join(', ');
                  return name.toLowerCase().includes(YOUR_SURNAME.toLowerCase())
                    ? `<strong>${name}</strong>`
                    : name;
                })
                .join(', ');
            }
          }
        } catch (e) {
          console.error(
            'Non è stato possibile caricare gli autori estesi per il DOI:',
            doi
          );
        }
      }

      // Costruisce la grafica sul sito
      const li = document.createElement('li');
      li.className = 'publication-card';

      li.innerHTML = `
        <p class="publication-title">${title}</p>
        <p class="publication-authors">${authorsText}</p>
        <p class="publication-meta">${journal}${journal ? ',' : ''} ${year}</p>
        ${
          doi
            ? `<p style="font-size: 0.9rem; margin-top: 0.5rem; color: #aaa;">DOI: <a href="https://doi.org/${doi}" target="_blank" rel="noopener noreferrer" style="color: #b3ffe5; text-decoration: underline;">${doi}</a></p>`
            : ''
        }
      `;

      list.appendChild(li);
    }
  } catch (error) {
    status.textContent = 'Error loading publications.';
    console.error(error);
  }
});

document.addEventListener('DOMContentLoaded', async () => {
  const list = document.getElementById('reviews-list');
  const ORCID = '0009-0004-0483-0463';

  try {
    const res = await fetch(`https://pub.orcid.org/v3.0/${ORCID}/peer-reviews`, {
      headers: { Accept: 'application/json' },
    });
    const data = await res.json();

    // Una voce per rivista, con il numero di revisioni
    const journals = await Promise.all(
      (data.group || []).map(async (g) => {
        const summaries = g['peer-review-group'].flatMap((pg) => pg['peer-review-summary']);
        const groupId = summaries[0]['review-group-id']; // es. "issn:0021-9991"
        let name = summaries[0]['convening-organization']?.name || groupId;

        if (groupId.startsWith('issn:')) {
          try {
            const cr = await fetch(`https://api.crossref.org/journals/${groupId.slice(5)}`);
            if (cr.ok) name = (await cr.json()).message.title;
          } catch (_) { /* se Crossref non risponde, resta il nome dell'editore */ }
        }
        return { name, count: summaries.length };
      })
    );

    list.innerHTML = '';
    if (!journals.length) {
      list.innerHTML = '<li>No review activity available.</li>';
      return;
    }
    journals
      .sort((a, b) => a.name.localeCompare(b.name))
      .forEach(({ name, count }) => {
        const li = document.createElement('li');
        li.textContent = count > 1 ? `${name} (${count} reviews)` : name;
        list.appendChild(li);
      });
  } catch (err) {
    list.innerHTML = '<li>Unable to load review activity.</li>';
    console.error(err);
  }
});

document.addEventListener('DOMContentLoaded', async () => {
  const box = document.getElementById('research-stats');
  if (!box) return;
  try {
    const res = await fetch('https://api.openalex.org/authors/orcid:0009-0004-0483-0463');
    if (!res.ok) throw new Error(res.status);
    const a = await res.json();
    const stats = {
      works: a.works_count,
      citations: a.cited_by_count,
      h: a.summary_stats?.h_index ?? 0,
      i10: a.summary_stats?.i10_index ?? 0,
    };
    box.querySelectorAll('[data-stat]').forEach((el) => countUp(el, stats[el.dataset.stat]));
    
  } catch (e) {
    box.remove(); // se OpenAlex non risponde, la sezione sparisce senza errori visibili
    console.error(e);
  }
});

function countUp(el, target, ms = 1200) {
  const t0 = performance.now();
  const step = (now) => {
    const p = Math.min((now - t0) / ms, 1);
    el.textContent = Math.round(target * (1 - Math.pow(1 - p, 3))); // ease-out
    if (p < 1) requestAnimationFrame(step);
  };
  requestAnimationFrame(step);
}


document.addEventListener('DOMContentLoaded', () => {
  const logo = document.querySelector('.logo a');
  if (!logo) return;
  const CAP = '<img src="assets/cap-accent.svg" alt="">';
  const onHome = /\/(index\.html)?$/.test(location.pathname);
  let clicks = 0, timer;

  logo.addEventListener('click', (e) => {
    if (!onHome) return; // sulle altre pagine il logo porta alla home come sempre
    e.preventDefault();
    window.scrollTo({ top: 0, behavior: 'smooth' });
    clicks++;
    clearTimeout(timer);
    timer = setTimeout(() => (clicks = 0), 1500);
    if (clicks === 5) { clicks = 0; capRain(); }
  });

  function capRain() {
    if (matchMedia('(prefers-reduced-motion: reduce)').matches) return;
    for (let i = 0; i < 30; i++) {
      const cap = document.createElement('div');
      cap.className = 'falling-cap';
      cap.innerHTML = CAP;
      cap.style.left = Math.random() * 95 + 'vw';
      cap.style.width = 30 + Math.random() * 40 + 'px';
      cap.style.animationDuration = 2.5 + Math.random() * 2 + 's';
      cap.style.animationDelay = Math.random() * 1.5 + 's';
      cap.style.setProperty('--spin', Math.random() * 720 - 360 + 'deg');
      document.body.appendChild(cap);
      cap.addEventListener('animationend', () => cap.remove());
    }
  }
});