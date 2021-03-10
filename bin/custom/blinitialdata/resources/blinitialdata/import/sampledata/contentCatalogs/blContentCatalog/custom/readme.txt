
Steps to add custom sampledata content catalog impexes :

1. Add custom impex file in - /blinitialdata/import/sampledata/contentCatalogs/blContentCatalog/custom
2. After adding impex file in custom folder we need to add the files entry in sequence impex - /blinitialdata/resources/blinitialdata/import/sampledata/contentCatalogs/sampledata_custom_content_data_sequence.impex
3. To add custom impex entry please use the below example to add entry in sampledata_custom_content_data_sequence.impex :
	#% impex.includeExternalData(InitialDataSystemSetup.class.getResourceAsStream("$contentCatalogsCustomPath/demo.impex"), "utf-8", 0, 0);
	
Note: For content catalog pages we need to follow the sequence defined below to import the impexes.  

1. Homepage related impexes
2. Content page related impexes
3. Category Page related impexes
4. Search Listing Page related impexes
5. Product Listing Page related impexes
6. Product Details Page related impexes
7. MyAccount Page related impexes
8. Cart Page related impexes
9. Checkout Page related impexes
10. Email Page related impexes