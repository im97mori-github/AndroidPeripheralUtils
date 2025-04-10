package org.im97mori.ble.android.peripheral.ui.device;

import android.app.Activity;
import android.app.Instrumentation;
import android.bluetooth.BluetoothAdapter;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.graphics.Bitmap;
import android.graphics.drawable.Animatable2;
import android.graphics.drawable.AnimatedVectorDrawable;
import android.graphics.drawable.Drawable;
import android.os.Build;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import androidx.annotation.NonNull;
import androidx.appcompat.widget.AppCompatImageView;
import androidx.arch.core.executor.testing.InstantTaskExecutorRule;
import androidx.core.view.MenuProvider;
import androidx.lifecycle.HasDefaultViewModelProviderFactory;
import androidx.lifecycle.Lifecycle;
import androidx.lifecycle.ViewModelProvider;
import androidx.test.core.app.ActivityScenario;
import androidx.test.core.app.ApplicationProvider;
import androidx.test.espresso.intent.Intents;
import androidx.test.espresso.matcher.ViewMatchers;
import androidx.test.filters.SdkSuppress;
import androidx.vectordrawable.graphics.drawable.Animatable2Compat;
import androidx.vectordrawable.graphics.drawable.AnimatedVectorDrawableCompat;
import com.google.android.material.appbar.MaterialToolbar;
import dagger.Module;
import dagger.Provides;
import dagger.hilt.InstallIn;
import dagger.hilt.android.qualifiers.ApplicationContext;
import dagger.hilt.android.testing.HiltAndroidRule;
import dagger.hilt.android.testing.HiltAndroidTest;
import dagger.hilt.android.testing.HiltTestApplication;
import dagger.hilt.android.testing.UninstallModules;
import dagger.hilt.components.SingletonComponent;
import io.reactivex.rxjava3.android.plugins.RxAndroidPlugins;
import io.reactivex.rxjava3.plugins.RxJavaPlugins;
import io.reactivex.rxjava3.schedulers.Schedulers;
import org.hamcrest.Description;
import org.hamcrest.TypeSafeMatcher;
import org.im97mori.ble.android.peripheral.R;
import org.im97mori.ble.android.peripheral.hilt.module.ViewModelFactoryFunctionModule;
import org.im97mori.ble.android.peripheral.test.FakeViewModelProviderFactoryFunction;
import org.im97mori.ble.android.peripheral.test.TestUtils;
import org.im97mori.ble.android.peripheral.ui.device.setting.DeviceSettingActivity;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.annotation.Config;

import javax.inject.Inject;
import javax.inject.Singleton;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;

import static android.view.MenuItem.SHOW_AS_ACTION_ALWAYS;
import static androidx.test.espresso.Espresso.onView;
import static androidx.test.espresso.action.ViewActions.click;
import static androidx.test.espresso.assertion.ViewAssertions.doesNotExist;
import static androidx.test.espresso.assertion.ViewAssertions.matches;
import static androidx.test.espresso.intent.Intents.intended;
import static androidx.test.espresso.intent.Intents.intending;
import static androidx.test.espresso.intent.matcher.IntentMatchers.*;
import static androidx.test.espresso.matcher.ViewMatchers.*;
import static org.im97mori.ble.android.peripheral.Constants.DeviceTypes.DEVICE_TYPE_BLOOD_PRESSURE_PROFILE;
import static org.im97mori.ble.android.peripheral.Constants.IntentKey.KEY_DEVICE_ID;
import static org.im97mori.ble.android.peripheral.Constants.IntentKey.KEY_DEVICE_TYPE;
import static org.im97mori.ble.android.peripheral.test.TestUtils.getCurrentMethodName;
import static org.im97mori.ble.android.peripheral.utils.Utils.stackLog;
import static org.junit.Assert.*;

// TODO recreate test
@HiltAndroidTest
@RunWith(RobolectricTestRunner.class)
@Config(instrumentedPackages = {
        // required to access final members on androidx.loader.content.ModernAsyncTask
        "androidx.loader.content"}
        , application = HiltTestApplication.class
        , sdk = Build.VERSION_CODES.LOLLIPOP)
@UninstallModules(ViewModelFactoryFunctionModule.class)
public class PeripheralActivityTest {

    @Module
    @InstallIn(SingletonComponent.class)
    interface FakeViewModelFactoryFunctionModule {
        @Singleton
        @Provides
        public static Function<HasDefaultViewModelProviderFactory, ViewModelProvider.Factory> bindViewModelProviderFactoryFunction() {
            FakeViewModelProviderFactoryFunction fakeViewModelProviderFactoryFunction = new FakeViewModelProviderFactoryFunction();
            fakeViewModelProviderFactoryFunction.setFakeViewModelClass(PeripheralViewModel.class, FakePeripheralViewModel.class);
            return fakeViewModelProviderFactoryFunction;
        }
    }

    @Rule(order = 1)
    public final HiltAndroidRule mHiltRule = new HiltAndroidRule(this);

    @Rule(order = 2)
    public final InstantTaskExecutorRule mInstantTaskRule = new InstantTaskExecutorRule();

    private ActivityScenario<PeripheralActivity> mScenario;

    private FakePeripheralViewModel mViewModel;

    @Inject
    @ApplicationContext
    Context mContext;

    @Inject
    Function<HasDefaultViewModelProviderFactory, ViewModelProvider.Factory> viewModelProviderFactoryFunction;

    @Before
    public void setUp() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        mHiltRule.inject();
        Intents.init();
    }

    @After
    public void tearDown() {
        Intents.release();
        mScenario.close();
    }

    @Test
    public void test_title_00001() {
        long id = 1;
        String deviceSettingName = "a";
        Intent intent = new Intent(mContext, PeripheralActivity.class);
        intent.putExtra(KEY_DEVICE_ID, id);

        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakePeripheralViewModel) new ViewModelProvider(activity).get(PeripheralViewModel.class));

        mViewModel.test_title_00001_String = deviceSettingName;
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        onView(withId(R.id.topAppBar)).check(matches(hasDescendant(withText(deviceSettingName))));
    }

    @Test
    public void test_root_container_visibility_00001() {
        long id = 1;
        Intent intent = new Intent(mContext, PeripheralActivity.class);
        intent.putExtra(KEY_DEVICE_ID, id);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakePeripheralViewModel) new ViewModelProvider(activity).get(PeripheralViewModel.class));

        onView(withId(R.id.rootContainer)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        onView(withId(R.id.rootContainer)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_deviceTypeImage_00001() {
        long id = 1;
        Intent intent = new Intent(mContext, PeripheralActivity.class);
        intent.putExtra(KEY_DEVICE_ID, id);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakePeripheralViewModel) new ViewModelProvider(activity).get(PeripheralViewModel.class));

        onView(withId(R.id.deviceTypeImage)).check(matches(new TypeSafeMatcher<View>() {
            @Override
            protected boolean matchesSafely(View item) {
                return ((AppCompatImageView) item).getDrawable() == null;
            }

            @Override
            public void describeTo(Description description) {
                description.appendText("device_type:" + null);
            }
        }));
    }

    @Test
    public void test_deviceTypeImage_00002() {
        long id = 1;
        Intent intent = new Intent(mContext, PeripheralActivity.class);
        intent.putExtra(KEY_DEVICE_ID, id);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakePeripheralViewModel) new ViewModelProvider(activity).get(PeripheralViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        onView(withId(R.id.deviceTypeImage)).check(matches(new TypeSafeMatcher<View>() {
            @Override
            protected boolean matchesSafely(View item) {
                Bitmap targetBitmap = TestUtils.getBitmap(((AppCompatImageView) item).getDrawable());
                Drawable drawable = item.getContext().getDrawable(R.drawable.medical_ketsuatsukei_aneroid);
                assertNotNull(drawable);
                Bitmap bitmap = TestUtils.getBitmap(drawable);
                return targetBitmap.sameAs(bitmap);
            }

            @Override
            public void describeTo(Description description) {
                description.appendText("device_type:" + R.drawable.medical_ketsuatsukei_aneroid);
            }
        }));
    }

    @Test
    public void test_deviceType_00001() {
        long id = 1;
        Intent intent = new Intent(mContext, PeripheralActivity.class);
        intent.putExtra(KEY_DEVICE_ID, id);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakePeripheralViewModel) new ViewModelProvider(activity).get(PeripheralViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        mScenario.onActivity(activity -> ((MaterialToolbar) activity.findViewById(R.id.topAppBar)).showOverflowMenu());
        onView(withText(R.string.menu_setting)).perform(click());

        intended(hasComponent(new ComponentName(ApplicationProvider.getApplicationContext(), DeviceSettingActivity.class)));
        intended(hasExtra(KEY_DEVICE_ID, id));
        intended(hasExtra(KEY_DEVICE_TYPE, DEVICE_TYPE_BLOOD_PRESSURE_PROFILE));
    }

    @Test
    public void test_deviceTypeTitle_00001() {
        long id = 1;
        Intent intent = new Intent(mContext, PeripheralActivity.class);
        intent.putExtra(KEY_DEVICE_ID, id);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakePeripheralViewModel) new ViewModelProvider(activity).get(PeripheralViewModel.class));

        onView(withId(R.id.deviceTypeNameTitle)).check(matches(withText(R.string.device_type)));
    }

    @Test
    public void test_deviceTypeName_00001() {
        long id = 1;
        Intent intent = new Intent(mContext, PeripheralActivity.class);
        intent.putExtra(KEY_DEVICE_ID, id);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakePeripheralViewModel) new ViewModelProvider(activity).get(PeripheralViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        onView(withId(R.id.deviceTypeName)).check(matches(withText(R.string.blood_pressure_profile)));
    }

    @Test
    public void test_observeIsReady_00001() {
        AtomicInteger count = new AtomicInteger(0);
        long id = 1;
        Intent intent = new Intent(mContext, PeripheralActivity.class);
        intent.putExtra(KEY_DEVICE_ID, id);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            mViewModel = (FakePeripheralViewModel) new ViewModelProvider(activity).get(PeripheralViewModel.class);
            MaterialToolbar materialToolbar = activity.findViewById(R.id.topAppBar);
            materialToolbar.addMenuProvider(new MenuProvider() {
                @Override
                public void onCreateMenu(@NonNull Menu menu, @NonNull MenuInflater menuInflater) {
                    count.incrementAndGet();
                }

                @Override
                public boolean onMenuItemSelected(@NonNull MenuItem menuItem) {
                    return false;
                }
            });
        });

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        assertEquals(3, count.get());
    }

    @Test
    public void test_observeIsStarted_00001() {
        CountDownLatch countDownLatch = new CountDownLatch(3);
        long id = 1;
        Intent intent = new Intent(mContext, PeripheralActivity.class);
        intent.putExtra(KEY_DEVICE_ID, id);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            mViewModel = (FakePeripheralViewModel) new ViewModelProvider(activity).get(PeripheralViewModel.class);
            MaterialToolbar materialToolbar = activity.findViewById(R.id.topAppBar);
            Drawable drawable = activity.findViewById(R.id.deviceTypeImage).getBackground();
            if (drawable instanceof AnimatedVectorDrawable) {
                ((AnimatedVectorDrawable) drawable).registerAnimationCallback(new Animatable2.AnimationCallback() {
                    @Override
                    public void onAnimationStart(Drawable drawable) {
                        countDownLatch.countDown();
                    }
                });
            } else if (drawable instanceof AnimatedVectorDrawableCompat) {
                ((AnimatedVectorDrawableCompat) drawable).registerAnimationCallback(new Animatable2Compat.AnimationCallback() {
                    @Override
                    public void onAnimationStart(Drawable drawable) {
                        countDownLatch.countDown();
                    }
                });
            }

            materialToolbar.addMenuProvider(new MenuProvider() {
                @Override
                public void onCreateMenu(@NonNull Menu menu, @NonNull MenuInflater menuInflater) {
                    countDownLatch.countDown();
                }

                @Override
                public boolean onMenuItemSelected(@NonNull MenuItem menuItem) {
                    return false;
                }
            });
        });

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        try {
            //noinspection ResultOfMethodCallIgnored
            countDownLatch.await(1, TimeUnit.SECONDS);
        } catch (InterruptedException e) {
            stackLog(e);
        }
        assertEquals(0, countDownLatch.getCount());
    }

    @Test
    public void test_observeIsStarted_00002() {
        CountDownLatch countDownLatch = new CountDownLatch(3);
        long id = 1;
        Intent intent = new Intent(mContext, PeripheralActivity.class);
        intent.putExtra(KEY_DEVICE_ID, id);
        mScenario = ActivityScenario.launch(intent);

        mScenario.onActivity(activity -> {
            mViewModel = (FakePeripheralViewModel) new ViewModelProvider(activity).get(PeripheralViewModel.class);

            mViewModel.mObserveSetupSubject.onNext("test_observeIsStarted_00001");

            MaterialToolbar materialToolbar = activity.findViewById(R.id.topAppBar);
            Drawable drawable = activity.findViewById(R.id.deviceTypeImage).getBackground();
            if (drawable instanceof AnimatedVectorDrawable) {
                ((AnimatedVectorDrawable) drawable).registerAnimationCallback(new Animatable2.AnimationCallback() {
                    @Override
                    public void onAnimationEnd(Drawable drawable) {
                        countDownLatch.countDown();
                    }
                });
            } else if (drawable instanceof AnimatedVectorDrawableCompat) {
                ((AnimatedVectorDrawableCompat) drawable).registerAnimationCallback(new Animatable2Compat.AnimationCallback() {
                    @Override
                    public void onAnimationEnd(Drawable drawable) {
                        countDownLatch.countDown();
                    }
                });
            }

            materialToolbar.addMenuProvider(new MenuProvider() {
                @Override
                public void onCreateMenu(@NonNull Menu menu, @NonNull MenuInflater menuInflater) {
                    countDownLatch.countDown();
                }

                @Override
                public boolean onMenuItemSelected(@NonNull MenuItem menuItem) {
                    return false;
                }
            });
        });

        mViewModel.test_observeIsStarted_00002();

        try {
            //noinspection ResultOfMethodCallIgnored
            countDownLatch.await(1, TimeUnit.SECONDS);
        } catch (InterruptedException e) {
            stackLog(e);
        }
        assertEquals(0, countDownLatch.getCount());
    }

    @Test
    public void test_observeIsBluetoothEnabled_00001() {
        AtomicInteger count = new AtomicInteger(0);
        long id = 1;
        Intent intent = new Intent(mContext, PeripheralActivity.class);
        intent.putExtra(KEY_DEVICE_ID, id);
        mScenario = ActivityScenario.launch(intent);

        mScenario.onActivity(activity -> {
            mViewModel = (FakePeripheralViewModel) new ViewModelProvider(activity).get(PeripheralViewModel.class);
            MaterialToolbar materialToolbar = activity.findViewById(R.id.topAppBar);
            materialToolbar.addMenuProvider(new MenuProvider() {
                @Override
                public void onCreateMenu(@NonNull Menu menu, @NonNull MenuInflater menuInflater) {
                    count.incrementAndGet();
                }

                @Override
                public boolean onMenuItemSelected(@NonNull MenuItem menuItem) {
                    return false;
                }
            });
        });

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        assertEquals(3, count.get());
    }

    @Test
    public void test_observeIsBluetoothEnabled_00002() {
        AtomicInteger count = new AtomicInteger(0);
        long id = 1;
        Intent intent = new Intent(mContext, PeripheralActivity.class);
        intent.putExtra(KEY_DEVICE_ID, id);
        mScenario = ActivityScenario.launch(intent);

        mScenario.onActivity(activity -> {
            mViewModel = (FakePeripheralViewModel) new ViewModelProvider(activity).get(PeripheralViewModel.class);
            MaterialToolbar materialToolbar = activity.findViewById(R.id.topAppBar);
            materialToolbar.addMenuProvider(new MenuProvider() {
                @Override
                public void onCreateMenu(@NonNull Menu menu, @NonNull MenuInflater menuInflater) {
                    count.incrementAndGet();
                }

                @Override
                public boolean onMenuItemSelected(@NonNull MenuItem menuItem) {
                    return false;
                }
            });
        });

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        assertEquals(2, count.get());
        assertTrue(mViewModel.mIsQuitCalled);
    }

    @Test
    public void test_menu_00001() {
        long id = 1;
        Intent intent = new Intent(mContext, PeripheralActivity.class);
        intent.putExtra(KEY_DEVICE_ID, id);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            mViewModel = (FakePeripheralViewModel) new ViewModelProvider(activity).get(PeripheralViewModel.class);
            MaterialToolbar materialToolbar = activity.findViewById(R.id.topAppBar);
            materialToolbar.getMenu().findItem(R.id.peripheralStart).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.peripheralStop).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.setting).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.delete).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.bluetooth_enable).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.bluetooth_disable).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
        });

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        onView(withId(R.id.peripheralStart)).check(matches(isNotEnabled()));
        onView(withId(R.id.peripheralStop)).check(matches(isEnabled()));
        onView(withId(R.id.setting)).check(matches(isNotEnabled()));
        onView(withId(R.id.delete)).check(matches(isNotEnabled()));
        onView(withId(R.id.bluetooth_enable)).check(matches(isNotEnabled()));
        onView(withId(R.id.bluetooth_disable)).check(matches(isEnabled()));

        mViewModel.mIsPeripheralReady = null;
        mViewModel.mIsPeripheralStarted = null;
        mViewModel.mIsBluetoothEnabled = null;
    }

    @Test
    public void test_menu_00002() {
        long id = 1;
        Intent intent = new Intent(mContext, PeripheralActivity.class);
        intent.putExtra(KEY_DEVICE_ID, id);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            mViewModel = (FakePeripheralViewModel) new ViewModelProvider(activity).get(PeripheralViewModel.class);
            MaterialToolbar materialToolbar = activity.findViewById(R.id.topAppBar);
            materialToolbar.getMenu().findItem(R.id.peripheralStart).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.peripheralStop).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.setting).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.delete).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.bluetooth_enable).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.bluetooth_disable).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
        });

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        onView(withId(R.id.peripheralStart)).check(matches(isEnabled()));
        onView(withId(R.id.peripheralStop)).check(matches(isNotEnabled()));
        onView(withId(R.id.setting)).check(matches(isEnabled()));
        onView(withId(R.id.delete)).check(matches(isEnabled()));
        onView(withId(R.id.bluetooth_enable)).check(matches(isNotEnabled()));
        onView(withId(R.id.bluetooth_disable)).check(matches(isEnabled()));

        mViewModel.mIsPeripheralReady = null;
        mViewModel.mIsPeripheralStarted = null;
        mViewModel.mIsBluetoothEnabled = null;
    }

    @Test
    public void test_menu_00003() {
        long id = 1;
        Intent intent = new Intent(mContext, PeripheralActivity.class);
        intent.putExtra(KEY_DEVICE_ID, id);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            mViewModel = (FakePeripheralViewModel) new ViewModelProvider(activity).get(PeripheralViewModel.class);
            MaterialToolbar materialToolbar = activity.findViewById(R.id.topAppBar);
            materialToolbar.getMenu().findItem(R.id.peripheralStart).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.peripheralStop).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.setting).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.delete).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.bluetooth_enable).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.bluetooth_disable).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
        });

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        onView(withId(R.id.peripheralStart)).check(matches(isNotEnabled()));
        onView(withId(R.id.peripheralStop)).check(matches(isNotEnabled()));
        onView(withId(R.id.setting)).check(matches(isNotEnabled()));
        onView(withId(R.id.delete)).check(matches(isNotEnabled()));
        onView(withId(R.id.bluetooth_enable)).check(matches(isNotEnabled()));
        onView(withId(R.id.bluetooth_disable)).check(matches(isEnabled()));

        mViewModel.mIsPeripheralReady = null;
        mViewModel.mIsPeripheralStarted = null;
        mViewModel.mIsBluetoothEnabled = null;
    }

    @Test
    public void test_menu_00004() {
        long id = 1;
        Intent intent = new Intent(mContext, PeripheralActivity.class);
        intent.putExtra(KEY_DEVICE_ID, id);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            mViewModel = (FakePeripheralViewModel) new ViewModelProvider(activity).get(PeripheralViewModel.class);
            MaterialToolbar materialToolbar = activity.findViewById(R.id.topAppBar);
            materialToolbar.getMenu().findItem(R.id.peripheralStart).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.peripheralStop).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.setting).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.delete).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.bluetooth_enable).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.bluetooth_disable).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
        });

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        onView(withId(R.id.peripheralStart)).check(matches(isNotEnabled()));
        onView(withId(R.id.peripheralStop)).check(matches(isNotEnabled()));
        onView(withId(R.id.setting)).check(matches(isEnabled()));
        onView(withId(R.id.delete)).check(matches(isEnabled()));
        onView(withId(R.id.bluetooth_enable)).check(matches(isEnabled()));
        onView(withId(R.id.bluetooth_disable)).check(matches(isNotEnabled()));

        mViewModel.mIsPeripheralReady = null;
        mViewModel.mIsPeripheralStarted = null;
        mViewModel.mIsBluetoothEnabled = null;
    }

    @Test
    @Config(instrumentedPackages = {
            // required to access final members on androidx.loader.content.ModernAsyncTask
            "androidx.loader.content"}
            , application = HiltTestApplication.class
            , sdk = Build.VERSION_CODES.TIRAMISU)
    public void test_menu_00005() {
        long id = 1;
        Intent intent = new Intent(mContext, PeripheralActivity.class);
        intent.putExtra(KEY_DEVICE_ID, id);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            mViewModel = (FakePeripheralViewModel) new ViewModelProvider(activity).get(PeripheralViewModel.class);
            MaterialToolbar materialToolbar = activity.findViewById(R.id.topAppBar);
            materialToolbar.getMenu().findItem(R.id.peripheralStart).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.peripheralStop).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.setting).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.delete).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.bluetooth_enable).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
        });

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        onView(withId(R.id.peripheralStart)).check(matches(isNotEnabled()));
        onView(withId(R.id.peripheralStop)).check(matches(isNotEnabled()));
        onView(withId(R.id.setting)).check(matches(isEnabled()));
        onView(withId(R.id.delete)).check(matches(isEnabled()));
        onView(withId(R.id.bluetooth_enable)).check(matches(isEnabled()));
        onView(withId(R.id.bluetooth_disable)).check(doesNotExist());

        mViewModel.mIsPeripheralReady = null;
        mViewModel.mIsPeripheralStarted = null;
        mViewModel.mIsBluetoothEnabled = null;
    }

    @Test
    public void test_menu_peripheralStart_00001() {
        long id = 1;
        Intent intent = new Intent(mContext, PeripheralActivity.class);
        intent.putExtra(KEY_DEVICE_ID, id);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            mViewModel = (FakePeripheralViewModel) new ViewModelProvider(activity).get(PeripheralViewModel.class);
            MaterialToolbar materialToolbar = activity.findViewById(R.id.topAppBar);
            materialToolbar.getMenu().findItem(R.id.peripheralStart).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.peripheralStop).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.setting).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.delete).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.bluetooth_enable).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.bluetooth_disable).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
        });

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        mScenario.onActivity(activity -> activity.findViewById(R.id.peripheralStart).performClick());

        assertTrue(mViewModel.mIsStartCalled);

        mViewModel.mIsPeripheralReady = null;
        mViewModel.mIsPeripheralStarted = null;
        mViewModel.mIsBluetoothEnabled = null;
    }

    @Test
    public void test_menu_peripheralStop_00001() {
        long id = 1;
        Intent intent = new Intent(mContext, PeripheralActivity.class);
        intent.putExtra(KEY_DEVICE_ID, id);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            mViewModel = (FakePeripheralViewModel) new ViewModelProvider(activity).get(PeripheralViewModel.class);
            MaterialToolbar materialToolbar = activity.findViewById(R.id.topAppBar);
            materialToolbar.getMenu().findItem(R.id.peripheralStart).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.peripheralStop).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.setting).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.delete).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.bluetooth_enable).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.bluetooth_disable).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
        });

        mViewModel.mIsQuitCallSuper = false;
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        onView(withId(R.id.peripheralStop)).perform(click());

        assertTrue(mViewModel.mIsQuitCalled);

        mViewModel.mIsPeripheralReady = null;
        mViewModel.mIsPeripheralStarted = null;
        mViewModel.mIsBluetoothEnabled = null;
    }

    @Test
    public void test_menu_setting_00001() {
        long id = 2;
        Intent intent = new Intent(mContext, PeripheralActivity.class);
        intent.putExtra(KEY_DEVICE_ID, id);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            mViewModel = (FakePeripheralViewModel) new ViewModelProvider(activity).get(PeripheralViewModel.class);
            MaterialToolbar materialToolbar = activity.findViewById(R.id.topAppBar);
            materialToolbar.getMenu().findItem(R.id.peripheralStart).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.peripheralStop).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.setting).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.delete).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.bluetooth_enable).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.bluetooth_disable).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
        });

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        onView(withId(R.id.setting)).perform(click());

        intended(hasComponent(new ComponentName(ApplicationProvider.getApplicationContext(), DeviceSettingActivity.class)));
        intended(hasExtra(KEY_DEVICE_ID, id));
        intended(hasExtra(KEY_DEVICE_TYPE, DEVICE_TYPE_BLOOD_PRESSURE_PROFILE));

        mViewModel.mIsPeripheralReady = null;
        mViewModel.mIsPeripheralStarted = null;
        mViewModel.mIsBluetoothEnabled = null;
    }

    @Test
    public void test_menu_delete_00001() {
        long id = 2;
        Intent intent = new Intent(mContext, PeripheralActivity.class);
        intent.putExtra(KEY_DEVICE_ID, id);
        mScenario = ActivityScenario.launchActivityForResult(intent);
        mScenario.onActivity(activity -> {
            mViewModel = (FakePeripheralViewModel) new ViewModelProvider(activity).get(PeripheralViewModel.class);
            MaterialToolbar materialToolbar = activity.findViewById(R.id.topAppBar);
            materialToolbar.getMenu().findItem(R.id.peripheralStart).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.peripheralStop).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.setting).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.delete).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.bluetooth_enable).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.bluetooth_disable).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
        });

        mViewModel.mIsQuitCallSuper = false;
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mViewModel.mFakeDeviceSettingRepository.mDeleteDeviceSettingConsumer = deviceSetting -> {
        };

        onView(withId(R.id.delete)).perform(click());

        assertTrue(mViewModel.mIsObserveDeleteDeviceSettingCalled);

        Instrumentation.ActivityResult activityResult = mScenario.getResult();
        assertEquals(Activity.RESULT_CANCELED, activityResult.getResultCode());
        mScenario.onActivity(activity ->
                assertTrue(activity.isFinishing()));

        mViewModel.mIsPeripheralReady = null;
        mViewModel.mIsPeripheralStarted = null;
        mViewModel.mIsBluetoothEnabled = null;
    }

    @Test
    public void test_menu_bluetooth_enable_00001() {
        long id = 2;
        Intent intent = new Intent(mContext, PeripheralActivity.class);
        intent.putExtra(KEY_DEVICE_ID, id);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            mViewModel = (FakePeripheralViewModel) new ViewModelProvider(activity).get(PeripheralViewModel.class);
            MaterialToolbar materialToolbar = activity.findViewById(R.id.topAppBar);
            materialToolbar.getMenu().findItem(R.id.peripheralStart).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.peripheralStop).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.setting).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.delete).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.bluetooth_enable).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.bluetooth_disable).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
        });

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        onView(withId(R.id.bluetooth_enable)).perform(click());

        assertTrue(mViewModel.mIsBluetoothEnableCalled);

        mViewModel.mIsPeripheralReady = null;
        mViewModel.mIsPeripheralStarted = null;
        mViewModel.mIsBluetoothEnabled = null;
    }

    @Test
    @Config(instrumentedPackages = {
            // required to access final members on androidx.loader.content.ModernAsyncTask
            "androidx.loader.content"}
            , application = HiltTestApplication.class
            , sdk = Build.VERSION_CODES.TIRAMISU)
    public void test_menu_bluetooth_enable_00002() {
        long id = 2;
        Intent intent = new Intent(mContext, PeripheralActivity.class);
        intent.putExtra(KEY_DEVICE_ID, id);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            mViewModel = (FakePeripheralViewModel) new ViewModelProvider(activity).get(PeripheralViewModel.class);
            MaterialToolbar materialToolbar = activity.findViewById(R.id.topAppBar);
            materialToolbar.getMenu().findItem(R.id.peripheralStart).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.peripheralStop).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.setting).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.delete).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.bluetooth_enable).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
        });

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        onView(withId(R.id.bluetooth_enable)).perform(click());

        intended(hasAction(BluetoothAdapter.ACTION_REQUEST_ENABLE));

        mViewModel.mIsPeripheralReady = null;
        mViewModel.mIsPeripheralStarted = null;
        mViewModel.mIsBluetoothEnabled = null;
    }

    @Test
    @SdkSuppress(maxSdkVersion = Build.VERSION_CODES.S_V2)
    public void test_menu_bluetooth_disable_00001() {
        long id = 2;
        Intent intent = new Intent(mContext, PeripheralActivity.class);
        intent.putExtra(KEY_DEVICE_ID, id);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            mViewModel = (FakePeripheralViewModel) new ViewModelProvider(activity).get(PeripheralViewModel.class);
            MaterialToolbar materialToolbar = activity.findViewById(R.id.topAppBar);
            materialToolbar.getMenu().findItem(R.id.peripheralStart).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.peripheralStop).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.setting).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.delete).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.bluetooth_enable).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.bluetooth_disable).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
        });

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        onView(withId(R.id.bluetooth_disable)).perform(click());

        assertTrue(mViewModel.mIsBluetoothDisableCalled);

        mViewModel.mIsPeripheralReady = null;
        mViewModel.mIsPeripheralStarted = null;
        mViewModel.mIsBluetoothEnabled = null;
    }

    @Test
    public void test_activity_result_00001() {
        Instrumentation.ActivityResult result = new Instrumentation.ActivityResult(Activity.RESULT_OK, null);
        intending(hasComponent(new ComponentName(ApplicationProvider.getApplicationContext(), DeviceSettingActivity.class))).respondWith(result);

        long id = 2;
        Intent intent = new Intent(mContext, PeripheralActivity.class);
        intent.putExtra(KEY_DEVICE_ID, id);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            mViewModel = (FakePeripheralViewModel) new ViewModelProvider(activity).get(PeripheralViewModel.class);
            MaterialToolbar materialToolbar = activity.findViewById(R.id.topAppBar);
            materialToolbar.getMenu().findItem(R.id.peripheralStart).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.peripheralStop).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.setting).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.delete).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.bluetooth_enable).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.bluetooth_disable).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
        });

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mViewModel.mObserveSetupSubject.onNext("test_activity_result_00001_1");

        onView(withId(R.id.setting)).perform(click());

        assertTrue(mViewModel.mIsClearCalled);

        assertTrue(mViewModel.mTestActivityResult_00001_1Called);

        mViewModel.mIsPeripheralReady = null;
        mViewModel.mIsPeripheralStarted = null;
        mViewModel.mIsBluetoothEnabled = null;
    }

    @Test
    public void test_activity_result_00002() {
        Instrumentation.ActivityResult result = new Instrumentation.ActivityResult(Activity.RESULT_CANCELED, null);
        intending(hasComponent(new ComponentName(ApplicationProvider.getApplicationContext(), DeviceSettingActivity.class))).respondWith(result);

        long id = 2;
        Intent intent = new Intent(mContext, PeripheralActivity.class);
        intent.putExtra(KEY_DEVICE_ID, id);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            mViewModel = (FakePeripheralViewModel) new ViewModelProvider(activity).get(PeripheralViewModel.class);
            MaterialToolbar materialToolbar = activity.findViewById(R.id.topAppBar);
            materialToolbar.getMenu().findItem(R.id.peripheralStart).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.peripheralStop).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.setting).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.delete).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.bluetooth_enable).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
            materialToolbar.getMenu().findItem(R.id.bluetooth_disable).setShowAsAction(SHOW_AS_ACTION_ALWAYS);
        });

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        onView(withId(R.id.setting)).perform(click());

        assertFalse(mViewModel.mIsClearCalled);

        mViewModel.mIsPeripheralReady = null;
        mViewModel.mIsPeripheralStarted = null;
        mViewModel.mIsBluetoothEnabled = null;
    }

    @Test
    public void test_onDestroy_00001() {
        long id = 2;
        Intent intent = new Intent(mContext, PeripheralActivity.class);
        intent.putExtra(KEY_DEVICE_ID, id);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakePeripheralViewModel) new ViewModelProvider(activity).get(PeripheralViewModel.class));

        mScenario.moveToState(Lifecycle.State.DESTROYED);

        assertTrue(mViewModel.mIsQuitCalled);

        mViewModel.mIsPeripheralReady = null;
        mViewModel.mIsPeripheralStarted = null;
        mViewModel.mIsBluetoothEnabled = null;
    }

    @Test
    public void test_recreate_root_container_visibility_00001() {
        long id = 1;
        Intent intent = new Intent(mContext, PeripheralActivity.class);
        intent.putExtra(KEY_DEVICE_ID, id);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakePeripheralViewModel) new ViewModelProvider(activity).get(PeripheralViewModel.class));

        onView(withId(R.id.rootContainer)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        onView(withId(R.id.rootContainer)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));

        mScenario.recreate();

        onView(withId(R.id.rootContainer)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        onView(withId(R.id.rootContainer)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_recreate_deviceTypeImage_00001() {
        long id = 1;
        Intent intent = new Intent(mContext, PeripheralActivity.class);
        intent.putExtra(KEY_DEVICE_ID, id);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakePeripheralViewModel) new ViewModelProvider(activity).get(PeripheralViewModel.class));

        onView(withId(R.id.deviceTypeImage)).check(matches(new TypeSafeMatcher<View>() {
            @Override
            protected boolean matchesSafely(View item) {
                return ((AppCompatImageView) item).getDrawable() == null;
            }

            @Override
            public void describeTo(Description description) {
                description.appendText("device_type:" + null);
            }
        }));

        mScenario.recreate();

        onView(withId(R.id.deviceTypeImage)).check(matches(new TypeSafeMatcher<View>() {
            @Override
            protected boolean matchesSafely(View item) {
                return ((AppCompatImageView) item).getDrawable() == null;
            }

            @Override
            public void describeTo(Description description) {
                description.appendText("device_type:" + null);
            }
        }));
    }

    @Test
    public void test_recreate_deviceTypeImage_00002() {
        long id = 1;
        Intent intent = new Intent(mContext, PeripheralActivity.class);
        intent.putExtra(KEY_DEVICE_ID, id);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakePeripheralViewModel) new ViewModelProvider(activity).get(PeripheralViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        onView(withId(R.id.deviceTypeImage)).check(matches(new TypeSafeMatcher<View>() {
            @Override
            protected boolean matchesSafely(View item) {
                Bitmap targetBitmap = TestUtils.getBitmap(((AppCompatImageView) item).getDrawable());
                Drawable drawable = item.getContext().getDrawable(R.drawable.medical_ketsuatsukei_aneroid);
                assertNotNull(drawable);
                Bitmap bitmap = TestUtils.getBitmap(drawable);
                return targetBitmap.sameAs(bitmap);
            }

            @Override
            public void describeTo(Description description) {
                description.appendText("device_type:" + R.drawable.medical_ketsuatsukei_aneroid);
            }
        }));

        mScenario.recreate();

        onView(withId(R.id.deviceTypeImage)).check(matches(new TypeSafeMatcher<View>() {
            @Override
            protected boolean matchesSafely(View item) {
                Bitmap targetBitmap = TestUtils.getBitmap(((AppCompatImageView) item).getDrawable());
                Drawable drawable = item.getContext().getDrawable(R.drawable.medical_ketsuatsukei_aneroid);
                assertNotNull(drawable);
                Bitmap bitmap = TestUtils.getBitmap(drawable);
                return targetBitmap.sameAs(bitmap);
            }

            @Override
            public void describeTo(Description description) {
                description.appendText("device_type:" + R.drawable.medical_ketsuatsukei_aneroid);
            }
        }));
    }

    @Test
    public void test_recreate_deviceTypeName_00001() {
        long id = 1;
        Intent intent = new Intent(mContext, PeripheralActivity.class);
        intent.putExtra(KEY_DEVICE_ID, id);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakePeripheralViewModel) new ViewModelProvider(activity).get(PeripheralViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        onView(withId(R.id.deviceTypeName)).check(matches(withText(R.string.blood_pressure_profile)));

        mScenario.recreate();

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
    }

}