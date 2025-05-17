document.addEventListener('DOMContentLoaded', function() {
  // Fetch Datamall data via fly.io
  Shiny.addCustomMessageHandler('fetch_datamall', function(params) {
    document.getElementById('upload_conf').innerHTML =
      '<span style=\"color:#2050C0; font-weight:bold;\"><i class=\"fas fa-hourglass-half\"></i> Importing from Datamall, please wait...</span>';
    var datamall_date_raw = document.getElementById("datamall_date").value;
    // Convert dates from "yyyy-mm" to the file format "yyyymm" (remove dash).
    var datamall_date = datamall_date_raw.replace("-", "");
    const encoded_account_key = encodeURIComponent(params.account_key);
    const csv_proxy_url = 'https://stc-brdv.fly.dev/datamall-proxy' +
      '?date=' + datamall_date +
      '&account_key=' + encoded_account_key;
    fetch(csv_proxy_url)
      .then(response => {
        if (!response.ok) throw new Error("Error fetching CSV from endpoint");
        return response.text();
      })
      .then(function(csv_text) {
        // Pass the CSV text to Shiny.
        Shiny.setInputValue('csv_data_in', { data1: csv_text });
        document.getElementById('upload_conf').innerHTML =
          '<span style="color:#00DD00; font-weight:bold;"><i class="fas fa-square-check"></i> File import from Datamall successful!</span>';
      })
      .catch(err => {
        console.error(err);
        document.getElementById('upload_conf').innerHTML =
          '<span style="color:#BB0000; font-weight:bold;"><i class="fas fa-triangle-exclamation"></i> ' + err.message + '</span>';
      });
  });

  // Fetch JSON data (data2 & data3) from BusRouter.
  Shiny.addCustomMessageHandler('fetch_busrouter', function(params) {
    document.getElementById('result_conf').innerHTML =
    '<span style=\"color:#2050C0; font-weight:bold;\"><i class=\"fas fa-hourglass-half\"></i> Importing from BusRouter, please wait...</span>';
    var json_urls = [
      'https://data.busrouter.sg/v1/services.json',
      'https://data.busrouter.sg/v1/stops.json'
    ];
    return Promise.all(
      json_urls.map(function(url) {
        return fetch(url).then(function(response) {
          if (!response.ok) {
            throw new Error('Your internet is dead. Good job.');
          }
          return response.json();
        });
      })
    ).then(function(return_json_data) {
      var json_data = {
        data2: return_json_data[0],
        data3: return_json_data[1]
      };
      Shiny.setInputValue('json_data_in', JSON.stringify(json_data));
      document.getElementById('result_conf').innerHTML =
        '<span style=\"color:#00DD00; font-weight:bold;\"><i class=\"fas fa-square-check\"></i> File import from BusRouter successful!</span>';
    })
    .catch(err => {
      console.error(err);
      document.getElementById('result_conf').innerHTML =
          '<span style=\"color:#BB0000; font-weight:bold;\"><i class=\"fas fa-triangle-exclamation\"></i> ' + err + '</span>';
    });
  });

  // Fetch data from Google Drive repo.
  Shiny.addCustomMessageHandler('fetch_drive', function(params) {
    document.getElementById('upload_conf').innerHTML =
      '<span style="color:#2050C0; font-weight:bold;"><i class="fas fa-hourglass-half"></i> Importing Datamall data from repository, please wait...</span>';
    document.getElementById('upload_conf2').innerHTML =
      '<span style="color:#2050C0; font-weight:bold;"><i class="fas fa-hourglass-half"></i> Importing BusRouter data from repository, please wait...</span>';
    // Get the selected dates from the inputs.
    var datamall_date_raw = document.getElementById("od_matrix_date").value;
    var bus_date_raw = document.getElementById("busrouter_date").value;
    // Convert dates from "yyyy-mm" to the file format "yyyymm" (remove dash).
    var datamall_date = datamall_date_raw.replace("-", "");
    var bus_date = bus_date_raw.replace("-", "");
    
    // Fetch data from registry and build the Google Drive URLs for the data.
    // Fetch Datamall CSV data via its dedicated endpoint:
    var datamall_repository = 'https://stc-brdv.fly.dev/repository/datamall?datamall_date=' + datamall_date;
    fetch(datamall_repository)
      .then(response => {
        if (!response.ok) {
          // If the response status indicates an error, parse the error
          return response.text().then(text => {
            throw new Error(text);
          });
        }
        return response.text();
      })
      .then(function(csv_data) {
        Shiny.setInputValue("csv_data_in", { data1: csv_data });
        document.getElementById('upload_conf').innerHTML =
          '<span style="color:#00DD00; font-weight:bold;"><i class="fas fa-square-check"></i> Datamall data import from repository successful!</span>';
      })
      .catch(error => {
        document.getElementById('upload_conf').innerHTML =
          '<span style="color:#BB0000; font-weight:bold;"><i class="fas fa-triangle-exclamation"></i> ' +
          error.message + '</span>';
      });

    var busrouter_repository = 'https://stc-brdv.fly.dev/repository/busrouter?bus_date=' + bus_date;
    fetch(busrouter_repository)
      .then(response => {
        if (!response.ok) {
          return response.text().then(text => {
            throw new Error(text);
          });
        }
        return response.text();
      })
      .then(function(busrouter_data) {
        Shiny.setInputValue('json_data_in', busrouter_data);
        document.getElementById('upload_conf2').innerHTML =
          '<span style="color:#00DD00; font-weight:bold;"><i class="fas fa-square-check"></i> BusRouter data import from repository successful!</span>';
      })
      .catch(error => {
        document.getElementById('upload_conf2').innerHTML =
          '<span style="color:#BB0000; font-weight:bold;"><i class="fas fa-triangle-exclamation"></i> ' +
          error.message + '</span>';
      });
  });

  // Clear cache upon refresh
  window.onbeforeunload = function() {
    // Clear session storage
    sessionStorage.clear();
    // Clear local storage (or specific cache keys)
    localStorage.clear();
    // Clear caches created via the Cache API
    if ('caches' in window) {
      caches.keys().then(function(names) {
        names.forEach(function(name) {
          caches.delete(name);
        });
      });
    }
  };

  // Auto-adjust iframe height.
  function send_height() {
    // Compute the document's height
    var height = document.documentElement.scrollHeight || document.body.scrollHeight;
    // Send the height to the parent window
    window.parent.postMessage({ iframe_height: height }, "*");
  }
  
  // Call it when loaded and possibly on resize/mutation if dynamic
  window.addEventListener("load", send_height);
  // Optionally update on resize or changes:
  window.addEventListener("resize", send_height);
})