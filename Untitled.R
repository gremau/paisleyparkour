devtools::install("~/GitHub/jerald/")

library("jerald")

eml <- EML::read_eml("~/_current/EDI_data_curation/knb-lter-jrn.210559002.1.xml")

template_dataset_metabase <- function(dsid){
  return(list("DataSetID" = as.integer(dsid),
    # DataSet table
    "DataSet" = data.frame(
      "DataSetID"= integer(),
      "Revision"= integer(),
      "Title"= character(),
      "PubDate"= character(),
      "Abstract"= character(),
      "ShortName"= character(),
      "UpdateFrequency"= character(),
      "MaintenanceDescription"= character(),
      "AbstractType"= character(),
      "BoilerplateSetting"= character()
    ),
    # DataSetEntities table
    "DataSetEntities" = data.frame(
      "DataSetID" = integer(),
      "EntitySortOrder" = integer(),
      "EntityName" = character(),
      "EntityType" = character(),
      "EntityDescription" = character(),
      "EntityRecords" = integer(),
      "FileType" = character(),
      "Urlhead" = character(),
      "Subpath" = character(),
      "FileName" = character(),
      "AdditionalInfo" = character(),
      "FileSize" = numeric(),
      "FileSizeUnits" = character(),
      "Checksum" = character()
    ),
    # DataSetMethods table
    "DataSetMethods" = data.frame(
      "DataSetID" =  integer(),
      "MethodStepID" =  integer(),
      "DescriptionType" =  character(),
      "Description" = character(),
      "Method_xml" = character()
    ),
    # DataSetAttributes
    "DataSetAttributes" = data.frame(
      "DataSetID" = integer(),
      "EntitySortOrder" = integer(),
      "ColumnPosition" = integer(),
      "ColumnName" = character(),
      "AttributeID" = character(),
      "AttributeLabel" = character(),
      "Description" = character(),
      "StorageType" = character(),
      "MeasurementScaleDomainID" = character(),
      "DateTimeFormatString" = character(), 
      "DateTimePrecision" = double(),
      "TextPatternDefinition" = character(),
      "Unit" = character(),
      "NumericPrecision" = double(), 
      "NumberType" = character(),
      "BoundsMinimum" = double(), 
      "BoundsMaximum" = double()
    ),
    # DataSetAttributeEnumeration
    "DataSetAttributeEnumeration" = data.frame(
      "DataSetID" = integer(),
      "EntitySortOrder" = integer(),
      "ColumnName" = character(),
      "CodeID" = character()
    ),
    # DataSetAttributeMissingCodes
    "DataSetAttributeMissingCodes" = data.frame(
      "DataSetID" = integer(),
      "EntitySortOrder" = integer(),
      "ColumnName" = character(),
      "MissingValueCodeID" = character()
    ),
    # ListCodes
    "ListCodes" = data.frame(
      "CodeID" = character(),
      "Code" = character(),
      "CodeExplanation" = character()
    ),
    # DataSetKeywords
    "DataSetKeywords" = data.frame(
      "DataSetID" = integer(),
      "Keyword" = character(),
      "ThesaurusID" = character()
    ),
    # ListKeywords
    "ListKeywords" = data.frame(
      "Keyword" = character(),
      "ThesaurusID" = character(),
      "KeywordType" = character()
    ),
    # DataSetPersonnel
    "DataSetPersonnel" = data.frame(
      "DataSetID" = integer(),
      "NameID" = character(),
      "AuthorshipOrder" = integer(),
      "AuthorshipRole" = character()
    ),
    # ListPeople
    "ListPeople" = data.frame(
      "NameID" = character(),
      "GivenName" = character(),
      "MiddleName" = character(),
      "SurName" = character(),
      "Organization" = character(),
      "Address1" = character(),
      "Address2" = character(),
      "Address3" = character(),
      "City" = character(),
      "State" = character(),
      "Country" = character(),
      "ZipCode" = character(),
      "Email" = character(),
      "WebPage" = character(),
      "Phone" = character(),
      "Position" = character()
    ),
    # ListPeopleID
    "ListPeopleID" = data.frame(
      "NameID" = character(),
      "IdentificationID" = character(),
      "IdentificationSystem" = character(),
      "IdentificationURL" = character()
    ),
    # DataSetTemporal
    "DataSetTemporal" = data.frame(
      "DataSetID" = integer(),
      "EntitySortOrder" = integer(),
      "BeginDate" = character(),
      "EndDate" = character(),
      "UseOnlyYear" = logical()
    ),
    # DataSetSites
    "DataSetSites" = data.frame(
      "DataSetID" = integer(),
      "EntitySortOrder" = integer(),
      "SiteID" = character(),
      "GeoCoverageSortOrder" = integer()
    ),
    # ListSites
    "ListSites" = data.frame(
      "SiteID" = character(),
      "SiteType" = character(),
      "SiteName" = character(),
      "SiteLocation" = character(),
      "SiteDescription" = character(),
      "Ownership" = character(),
      "ShapeType" = character(),
      "CenterLon" = double(),
      "CenterLat" = double(),
      "WBoundLon" = double(),
      "EBoundLon" = double(),
      "SBoundLat" = double(),
      "NBoundLat" = double(),
      "AltitudeMin" = double(),
      "AltitudeMax" = double(),
      "AltitudeUnit" = character()
    )
  )
  )
}

test_mb <- template_dataset_metabase(201999999)

test <- format_DataSet(201999999, eml)
rbind(test$DataSet, test_mb$DataSet)
