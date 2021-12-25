package org.im97mori.ble.android.peripheral.ui.main;

import android.content.Context;

import androidx.annotation.NonNull;

import org.im97mori.ble.android.peripheral.hilt.datasource.DeviceDataSource;
import org.im97mori.ble.android.peripheral.hilt.repository.DeviceRepository;
import org.im97mori.ble.android.peripheral.room.Device;

import java.util.List;
import java.util.function.Consumer;

import javax.inject.Inject;

import dagger.hilt.android.qualifiers.ApplicationContext;
import io.reactivex.rxjava3.core.Completable;
import io.reactivex.rxjava3.core.Flowable;
import io.reactivex.rxjava3.processors.PublishProcessor;

public class FakeDeviceRepository extends DeviceRepository {

    PublishProcessor<List<Device>> getLoadDevicesProcessor = PublishProcessor.create();

    private Consumer<Boolean> deleteAllDevicesConsumer;
    @Inject
    public FakeDeviceRepository(@NonNull DeviceDataSource deviceDataSource, @NonNull @ApplicationContext Context context) {
        super(deviceDataSource, context);
    }

    @NonNull
    @Override
    public Flowable<List<Device>> loadDevices() {
        return getLoadDevicesProcessor;
    }

    @NonNull
    @Override
    public Completable deleteAllDevices() {
        if (deleteAllDevicesConsumer != null) {
            deleteAllDevicesConsumer.accept(true);
        }
        return super.deleteAllDevices();
    }

    public void setDeleteAllDevicesConsumer(@NonNull Consumer<Boolean> consumer) {
        deleteAllDevicesConsumer = consumer;
    }

}
