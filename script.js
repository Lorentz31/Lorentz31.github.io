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

