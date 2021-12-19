package org.im97mori.ble.android.peripheral.module;

import androidx.annotation.NonNull;

import org.im97mori.ble.android.peripheral.datasource.DeviceDataSource;

import javax.inject.Singleton;

import dagger.Module;
import dagger.Provides;

@Module
public class DeviceModule {

    private final DeviceDataSource mDeviceDataSource;

    public DeviceModule(@NonNull DeviceDataSource deviceDataSource) {
        mDeviceDataSource = deviceDataSource;
    }

    @Provides
    @Singleton
    public DeviceDataSource provideDeviceDataSource() {
        return mDeviceDataSource;
    }

}
