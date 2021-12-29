package org.im97mori.ble.android.peripheral.ui.device;

import static org.im97mori.ble.android.peripheral.Constants.IntentKey.KEY_DEVICE_ID;
import static org.im97mori.ble.android.peripheral.Constants.IntentKey.VALUE_DEVICE_ID_UNSAVED;

import android.graphics.drawable.AnimatedVectorDrawable;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;

import androidx.activity.result.ActivityResultLauncher;
import androidx.annotation.NonNull;
import androidx.core.util.Pair;
import androidx.core.view.MenuProvider;
import androidx.room.rxjava3.EmptyResultSetException;

import org.im97mori.ble.android.peripheral.R;
import org.im97mori.ble.android.peripheral.databinding.PeripheralActivityBinding;
import org.im97mori.ble.android.peripheral.ui.BaseActivity;
import org.im97mori.ble.android.peripheral.ui.device.setting.DeviceSettingLauncherContract;
import org.im97mori.ble.android.peripheral.utils.MockableViewModelProvider;
import org.im97mori.ble.profile.peripheral.AbstractProfileMockCallback;
import org.im97mori.stacklog.LogUtils;

import dagger.hilt.android.AndroidEntryPoint;

@AndroidEntryPoint
public class PeripheralActivity extends BaseActivity {

    private PeripheralViewModel mViewModel;

    private AbstractProfileMockCallback mCallback;

    private PeripheralActivityBinding mBinding;

    private Integer mDeviceType;

    private final ActivityResultLauncher<Pair<Long, Integer>> mStartDeviceSettingActivity = registerForActivityResult(new DeviceSettingLauncherContract(), result -> {
        if (result) {
            if (mCallback != null) {
                mCallback.quit();
                ((AnimatedVectorDrawable) mBinding.deviceTypeImage.getBackground()).stop();
                mCallback = null;
            }
            mBinding.topAppBar.invalidateMenu();
            fetch();
        }
    });

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        mViewModel = new MockableViewModelProvider(this).get(PeripheralViewModel.class);

        mBinding = PeripheralActivityBinding.inflate(getLayoutInflater());
        setContentView(mBinding.getRoot());

        mViewModel.observeTitle(this, mBinding.topAppBar::setTitle);
        mViewModel.observeTypeImageRes(this, mBinding.deviceTypeImage::setImageResource);
        mViewModel.observeDeviceType(this, integer -> mDeviceType = integer);
        mViewModel.observeDeviceTypeName(this, mBinding.deviceTypeName::setText);

        mBinding.topAppBar.addMenuProvider(new MenuProvider() {
            @Override
            public void onCreateMenu(@NonNull Menu menu, @NonNull MenuInflater menuInflater) {
                if (mCallback == null) {
                    menu.findItem(R.id.peripheralStart).setEnabled(false);
                    menu.findItem(R.id.peripheralStop).setEnabled(false);
                    menu.findItem(R.id.setting).setEnabled(false);
                    menu.findItem(R.id.delete).setEnabled(false);
                } else {
                    if (mCallback.isStarted()) {
                        menu.findItem(R.id.peripheralStart).setEnabled(false);
                        menu.findItem(R.id.peripheralStop).setEnabled(true);
                        menu.findItem(R.id.setting).setEnabled(false);
                        menu.findItem(R.id.delete).setEnabled(false);
                    } else {
                        menu.findItem(R.id.peripheralStart).setEnabled(true);
                        menu.findItem(R.id.peripheralStop).setEnabled(false);
                        menu.findItem(R.id.setting).setEnabled(true);
                        menu.findItem(R.id.delete).setEnabled(true);
                    }
                }
            }

            @Override
            public boolean onMenuItemSelected(@NonNull MenuItem menuItem) {
                boolean result;
                if (R.id.peripheralStart == menuItem.getItemId()) {
                    mCallback.start();
                    ((AnimatedVectorDrawable) mBinding.deviceTypeImage.getBackground()).start();
                    mBinding.topAppBar.invalidateMenu();
                    result = true;
                } else if (R.id.peripheralStop == menuItem.getItemId()) {
                    mCallback.quit();
                    ((AnimatedVectorDrawable) mBinding.deviceTypeImage.getBackground()).stop();
                    mBinding.topAppBar.invalidateMenu();
                    result = true;
                } else if (R.id.setting == menuItem.getItemId()) {
                    mStartDeviceSettingActivity.launch(Pair.create(getIntent().getLongExtra(KEY_DEVICE_ID, VALUE_DEVICE_ID_UNSAVED)
                            , mDeviceType));
                    result = true;
                } else if (R.id.delete == menuItem.getItemId()) {
                    mDisposable.add(mViewModel.deleteDevice(getIntent()).subscribe(() -> finish()));
                    result = true;
                } else {
                    result = false;
                }
                return result;
            }
        });
    }

    @Override
    protected void onStart() {
        super.onStart();
        if (mCallback == null) {
            fetch();
        }
    }

    @Override
    protected void onDestroy() {
        if (mCallback != null) {
            mCallback.quit();
            ((AnimatedVectorDrawable) mBinding.deviceTypeImage.getBackground()).stop();
        }
        super.onDestroy();
    }

    private void fetch() {
        if (mCallback == null) {
            mDisposable.add(mViewModel.setup(getIntent())
                    .subscribe(profileMockCallback -> {
                        mCallback = profileMockCallback;
                        mBinding.rootContainer.setVisibility(View.VISIBLE);
                        mBinding.topAppBar.invalidateMenu();
                    }, throwable -> {
                        LogUtils.stackLog(throwable.getMessage());
                        if (throwable instanceof EmptyResultSetException) {
                            finish();
                        }
                    }));
        }
    }

}