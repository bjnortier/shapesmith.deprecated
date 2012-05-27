# Upgrading 

Shapesmith uses a cache to store the solid model representations. This cache may become outdated when upgrading to a newer version of Shapesmith, and these files need to be deleted.

If you have a local installation, there will be a '_brep' directory in either 'node/db/_brep' or 'nodes/apps/api/db/_brep'. You can safely delete the files within the '_brep' directory without losing your models (but it is still a good idea to back up your model files in the 'local' directory).

