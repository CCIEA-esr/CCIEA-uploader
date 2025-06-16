``` mermaid
%% label: mermaid
%% fig-cap: "CCIEA Uploader Code Flowchart"
graph TD
subgraph <b>CCIEA Uploader code flowchart</b>
direction LR
 subgraph <br>
 direction TB
   Z(main.yml) -. Daily GitHub Action .- B(get_status.R)
  B ---> C[[generate_file_status]] --> F[/uploader_status.json/] --> N(status.qmd)
  I[\Data Provider<br>ESR year Folder\] --> C
  J[\Data Figure Naming <br> Conventions\] -->E
  K[\Metadata\] --> D
  B ---> D[[get_indices]] --> G[/items_meta.json/] --> M(uploader.qmd) -- File check --> P(Data Provider<br>upload Folder) -- File clean --> R(Data Provider<br>ESR year Folder)
  B ---> E[[get_file_conventions]] -->H[/cciea_naming_conventions.json/] --> M
  N & M -.- Q(_quarto.yml) --> X(GitHub Uploader<br> Web Page)
  end
  subgraph <b>Legend</b>
  direction LR
  L1(yaml) ~~~ L2(R code) ~~~ L3(Google Drive) ~~~ L4(Google Sheet) ~~~ L5(Data) ~~~ L6(Quarto doc)
  end
  end
  classDef yaml fill:#ffffc5,stroke:#333,stroke-width:2px,color:#000;
  classDef drive fill:#f96,stroke:#333,stroke-width:2px,color:#000;
  classDef sheet fill:#d0f0c0,stroke:#333,stroke-width:2px,color:#000;
  classDef rcode fill:#ffcccb,stroke:#333,stroke-width:2px,color:#000;
  classDef datain fill:#add8e6,stroke:#333,stroke-width:2px,color:#000;
  classDef qmd fill:#d6a9d6,stroke:#333,stroke-width:2px,color:#000;
  classDef htm fill:#4682b4,stroke:#333,stroke-width:2px,color:#fff;
     class Z,Q,L1 yaml
     class I,P,R,L3 drive
     class J,K,L4 sheet
     class B,C,D,E,L2 rcode
     class F,G,H,L5 datain
     class M,N,L6 qmd
     class X htm
  
```
