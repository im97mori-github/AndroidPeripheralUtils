package org.im97mori.ble.android.peripheral.hilt.module;

import androidx.annotation.NonNull;

import org.im97mori.ble.android.peripheral.hilt.datasource.DeviceDataSource;

//@Module
public class DeviceModule {

    private final DeviceDataSource mDeviceDataSource;

    public DeviceModule(@NonNull DeviceDataSource deviceDataSource) {
        mDeviceDataSource = deviceDataSource;
    }

//    @Provides
//    @Singleton
    public DeviceDataSource provideDeviceDataSource() {
        return mDeviceDataSource;
    }

}
