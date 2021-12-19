package org.im97mori.ble.android.peripheral.hilt.module;

import androidx.annotation.NonNull;

import org.im97mori.ble.android.peripheral.hilt.datasource.ResourceTextSource;

//@Module
public class TextModule {

    private final ResourceTextSource mResourceTextSource;

    public TextModule(@NonNull ResourceTextSource resourceTextSource) {
        mResourceTextSource = resourceTextSource;
    }

//    @Provides
//    @Singleton
    public ResourceTextSource provideResourceTextSource() {
        return mResourceTextSource;
    }

}
