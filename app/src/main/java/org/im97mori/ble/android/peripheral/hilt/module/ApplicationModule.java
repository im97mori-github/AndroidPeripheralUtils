package org.im97mori.ble.android.peripheral.hilt.module;

import com.google.gson.Gson;

import org.im97mori.ble.android.peripheral.utils.Utils;

import javax.inject.Singleton;

import dagger.Module;
import dagger.Provides;
import dagger.hilt.InstallIn;
import dagger.hilt.components.SingletonComponent;

@Module
@InstallIn(SingletonComponent.class)
public class ApplicationModule {

    @Provides
    @Singleton
    public Gson provideGson() {
        return Utils.createGsonInstance();
    }

}
