package org.im97mori.ble.android.peripheral.ui.main;

import androidx.annotation.NonNull;

import org.im97mori.ble.android.peripheral.hilt.repository.DeviceRepository;
import org.im97mori.ble.android.peripheral.room.Device;

import java.util.List;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import io.reactivex.rxjava3.core.Completable;
import io.reactivex.rxjava3.functions.Action;
import io.reactivex.rxjava3.functions.Consumer;
import io.reactivex.rxjava3.processors.PublishProcessor;

@HiltViewModel
public class FakeMainViewModel extends MainViewModel {

    public final PublishProcessor<List<Device>> mObserveDevicesProcessor = PublishProcessor.create();

    public Action mObserveDeleteAllDevicesAction;

    @Inject
    FakeMainViewModel(@NonNull DeviceRepository deviceRepository) {
        super(deviceRepository);
    }

    @Override
    public void observeDevices(io.reactivex.rxjava3.functions.Consumer<List<Device>> onNext, @NonNull Consumer<Throwable> onError) {
        mDisposable.add(mObserveDevicesProcessor.subscribe(onNext, onError));
    }

    @Override
    public void observeDeleteAllDevices(@NonNull Action onComplete, @NonNull Consumer<Throwable> onError) {
        if (mObserveDeleteAllDevicesAction == null) {
            super.observeDeleteAllDevices(onComplete, onError);
        } else {
            mDisposable.add(Completable.fromAction(mObserveDeleteAllDevicesAction).subscribe(onComplete, onError));
        }
    }

}