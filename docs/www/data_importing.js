document.addEventListener('DOMContentLoaded', function() {
  // Fetch Datamall data via Netlify
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
          return response.text();
        });
      })
    ).then(function(return_json_data) {
      var json_data = {
        data2: return_json_data[0],
        data3: return_json_data[1]
      };
      Shiny.setInputValue('json_data_in', json_data);
      //document.getElementById('result_conf').innerHTML =
        //'<span style=\"color:#00DD00; font-weight:bold;\"><i class=\"fas fa-square-check\"></i> File import successful!</span>';
    })
    .catch(err => {
      console.error(err);
      document.getElementById('result_conf').innerHTML =
          '<span style=\"color:#BB0000; font-weight:bold;\"><i class=\"fas fa-triangle-exclamation\"></i> ' + err + '</span>';
    });
  });

  // Fetch data from Google Drive repo.
  Shiny.addCustomMessageHandler('fetch_drive', function(params) {
    // Get the selected dates from the inputs.
    var datamall_date_raw = document.getElementById("od_matrix_date").value;
    var bus_date_raw = document.getElementById("busrouter_date").value;
    
    // Convert dates from "yyyy-mm" to the file format "yyyymm" (remove dash).
    var datamall_date = datamall_date_raw.replace("-", "");
    var bus_date = bus_date_raw.replace("-", "");
    
    // Fetch data from registry and build the Google Drive URLs for the data.
    var registry_url = "https://drive.google.com/uc?export=download&id=1aaGg28eZDRh8V87x8aX4CMySllr2LrGP";
    fetch(registry_url)
    .then(response => response.json())
    .then(registry => {
      // Build the Google Drive URL for each file based on the registry.
      var od_link = "https://drive.google.com/uc?export=download&id=" +
        registry.datamall['origin_destination_bus_' + datamall_date + '.csv'];
      var repo_json_urls = [
        "https://drive.google.com/uc?export=download&id=" +
          registry.busrouter.services['services_' + bus_date + '.json'],
        "https://drive.google.com/uc?export=download&id=" +
          registry.busrouter.stops['stops_' + bus_date + '.json']
      ];
      
      // Now that we have od_link and repo_json_urls, fetch the Datamall CSV data.
      fetch(od_link)
        .then(response => {
          if (!response.ok) {
            throw new Error("There is no Datamall data for the specified date.");
          }
          return response.text();  // Get the raw file as text.
        })
        .then(function(csv_text) {
          console.log("Datamall data successfully imported!");
          // Pass the imported data into Shiny
          Shiny.setInputValue("csv_data_in", { data1: csv_text });
          document.getElementById('upload_conf').innerHTML =
            '<span style="color:#00DD00; font-weight:bold;"><i class="fas fa-square-check"></i> Datamall data import from repository successful!</span>';
        })
        .catch(err => {
          console.error("Error importing Datamall data:", err);
          document.getElementById('upload_conf').innerHTML =
            '<span style="color:#BB0000; font-weight:bold;"><i class="fas fa-triangle-exclamation"></i> There seems to be no Datamall data on the repository for ' + datamall_date_raw + '...</span>';
        });
      
      // Now fetch the BusRouter JSON data.
      Promise.all(
        repo_json_urls.map(function(url) {
          return fetch(url)
            .then(function(response) {
              if (!response.ok) {
                throw new Error('Error fetching BusRouter data.');
              }
              return response.text();
            });
        })
      )
      .then(function(return_json_data) {
        var json_data = {
          data2: return_json_data[0],
          data3: return_json_data[1]
        };
        Shiny.setInputValue('json_data_in', json_data);
        document.getElementById('upload_conf2').innerHTML =
          '<span style="color:#00DD00; font-weight:bold;"><i class="fas fa-square-check"></i> BusRouter data import from repository successful!</span>';
      })
      .catch(err => {
        console.error(err);
        document.getElementById('upload_conf2').innerHTML =
          '<span style="color:#BB0000; font-weight:bold;"><i class="fas fa-triangle-exclamation"></i> There seems to be no BusRouter data on the repository for ' + bus_date_raw + '...</span>';
      });
    })
    .catch(error => {
      console.error("Error fetching registry:", error);
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